#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define u8  uint8_t
#define u32 uint32_t
#define u64 uint64_t

#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define BAR_INDEX 255
#define QUADRANT_SIZE 6
#define BOARD_SIZE (4 * QUADRANT_SIZE)
#define NUM_CHECKERS 15
#define NUM_DICE 2

//FILE* log_file;
typedef enum {
  RED,
  WHITE,
} Color;

typedef struct {
  u8* white;
  u8* red;
  u8 white_bar;
  u8 red_bar;
} Board;

u64 xorshift_state = 8425289453374772393;

u64 random_number() {
  u64 x = xorshift_state;
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  xorshift_state = x;
  return x * UINT64_C(2685821657736338717);
}

void seed_prng(void) {
  struct timespec t;
  for(int i =0; i < 4; i++) {
    assert(!clock_gettime(CLOCK_MONOTONIC, &t));
    xorshift_state ^= (u64)(t.tv_nsec << 32);
    xorshift_state ^= (u64)(t.tv_sec);
    xorshift_state += random_number();
  }
}

int generate_moves(u8* from, u8* to, Board* b, Color c, u8 die) {
  if (die < 1 || die > 6) {
    //fprintf(log_file, "die: %d\n", die);
  }
  assert(1 <= die && die <= 6);
  u8 bar;
  u8 *u, *v;
  if (c == WHITE) {
    bar = b->white_bar;
    u = b->white;
    v = b->red;
  } else {
    bar = b->red_bar;
    u = b->red;
    v = b->white;
  }
  u8 move_index = 0;
  if (bar) {
    u8 pos = -1 + (int)die;
    assert(0 <= pos && pos < BOARD_SIZE);
    u8 height = v[(BOARD_SIZE - 1) - pos];
    if (height <= 1) {
        assert(move_index < BOARD_SIZE);
        from[move_index] = BAR_INDEX;
        to[move_index++] = pos;
    }
    return move_index;
  }
  int min;
  for (min = 0; min < BOARD_SIZE && u[min] == 0; min++) {
  }
  if (min == BOARD_SIZE) {
    return 0;
  }
  bool bearing_off = min >= 3 * QUADRANT_SIZE;
  for(int i = min; i < BOARD_SIZE; i++) {
    if (u[i] != 0) {
      u8 pos = i + die;
      if (pos < BOARD_SIZE) {
        u8 height = v[(BOARD_SIZE - 1) - pos];
        if (height <= 1) {
          from[move_index] = i;
          to[move_index++] = pos;
        }
      } else if (bearing_off) {
        if ((pos == BOARD_SIZE) || (i == min)) {
          from[move_index] = i;
          to[move_index++] = BOARD_SIZE;
        }
      }
    }
  }
  return move_index;
}

void apply_move(Board* b, Color c, u8 from, u8 to) {
  u8 *bar, *v_bar;
  u8 *u, *v;
  if (c == WHITE) {
    bar = &(b->white_bar);
    v_bar = &(b->red_bar);
    u = b->white;
    v = b->red;
  } else {
    bar = &(b->red_bar);
    v_bar = &(b->white_bar);
    u = b->red;
    v = b->white;
  }
  if (from == BAR_INDEX) {
    *bar -= 1;
  } else {
    u[from] -= 1;
  }
  if (to < BOARD_SIZE) {
    u[to] += 1;
    u8* v_to = v + ((BOARD_SIZE - 1) - to);
    if (*v_to) {
      assert(*v_to == 1);
      *v_bar += 1;
      *v_to = 0;
    }
  }
}

Color invert(Color c) {
  if (c == WHITE) {
    return RED;
  }
  return WHITE;
}

Color playout(Board* b, Color c) {
    LOOP: while (1) {
    int r = random_number();
    u8 die0 = (u32)r % 6 + 1;
    r >>= 4;
    u8 die1 = (u32)r % 6 + 1;
    r >>= 4;
    u8 from[BOARD_SIZE];
    u8 to[BOARD_SIZE];
    if (die0 == die1) {
      for(int i = 0; i < 4; i++) {
        u8 moves = generate_moves(from, to, b, c, die0);
        if (moves == 0) {
          break;
        }
        int j = (u32)r % moves;
        r >>= 4;
        apply_move(b, c, from[j], to[j]);
      }
    } else {
      for(int i = 0; i < 2; i++) {
        u8 moves = generate_moves(from, to, b, c, die0);
        if (moves != 0) {
          int j = (u8)r % moves;
          r >>= 4;
          apply_move(b, c, from[j], to[j]);
        }
        die0 = die1;
      }
    }
    u8* x;
    if (c == WHITE) {
      x = b->white;
    } else {
      x = b->red;
    }
    for(int i = 0; i < BOARD_SIZE; i++) {
      if (x[i]) {
        c = invert(c);
        goto LOOP;
      }
    }
    return c;
  }
}

u64 get_time_in_usecs(void) {
  struct timespec t;
  assert(!clock_gettime(CLOCK_MONOTONIC, &t));
  return (u64)(t.tv_sec) * 1000000 + (u64)(t.tv_nsec) / 1000;
}

int COUNTER = 0;

typedef struct {
  u8 from;
  u8 to;
} Move;

Move select_move(Board* b, Color c, u8 die, u32 time_limit) {
  u64 t0 = get_time_in_usecs();
  u8 from[BOARD_SIZE];
  u8 to[BOARD_SIZE];
  u8 moves = generate_moves(from, to, b, c, die);
  int scores[moves];
  memset(scores, 0, moves * sizeof(int));
  Board b1;
  u8 white[BOARD_SIZE];
  u8 red[BOARD_SIZE];
  b1.white = white;
  b1.red = red;
  int playout_block = 32;
  u64 t1 = t0;
  do {
    for(int i = 0; i < playout_block; i++) {
      for(int move = 0; move < moves; move++) {
        memcpy(b1.white, b->white, BOARD_SIZE);
        memcpy(b1.red, b->red, BOARD_SIZE);
        b1.white_bar = b->white_bar;
        b1.red_bar = b->red_bar;
        apply_move(&b1, c, from[move], to[move]);
        Color win_color = playout(&b1, invert(c));
        if (win_color == c) {
          scores[move] += 1;
        }
      }
    }
    COUNTER += playout_block;
    u64 t2 = get_time_in_usecs();
    u64 dt = t2 - t1;
    t1 = t2;
    u64 rate = (256 * playout_block) / dt; // Prescale to increase resolution.
    u64 time_remaining = t0 + time_limit - t2;
    playout_block = (rate * time_remaining) / (2 * 256);
  } while (playout_block > 1);
  //printf("dt: %lu\n", t1 - t0);
  int score = 0;
  int m = 0;
  for(int move = 0; move < moves; move++) {
    //printf("(%d, %f, %d, %d)", move, (float)scores[move] / playouts, from[move], to[move]); if (scores[move] >= score) {
      m = move;
      score = scores[move];
  }
  if (die0_used != NULL) {
      *die0_used = m < moves0;
  }
  return (Move){from[m], to[m]};
}

void parse_dice(char* s0, u8* die0, u8* die1) {
  assert(s0[0] == 'd');
  char* s1;
  *die0 = strtol(s0 + 1, &s1, 10);
  *die1 = strtol(s1, NULL, 10);
  return;
}

void send_move(Move m) {
  //fprintf(log_file, "sending move\n");
  printf("m %d %d\n", m.from, m.to);
  //fprintf(log_file, "sent move\n");
}

int main(void) {
  seed_prng();
  //char log_file_name[1024];
  //sprintf((char*)(log_file_name), "log%lu.txt", random_number());
  //log_file = fopen(log_file_name, "w+");
  //setvbuf(log_file, NULL, _IOLBF, 1024);
  setvbuf(stdout, NULL, _IOLBF, 1024);
  setvbuf(stdin, NULL, _IOLBF, 1024);
  Board b = {0};
  b.white = calloc(BOARD_SIZE, 1);
  b.red = calloc(BOARD_SIZE, 1);
  b.white[0] = 2;
  b.white[11] = 5;
  b.white[16] = 3;
  b.white[18] = 5;
  b.red[0] = 2;
  b.red[11] = 5;
  b.red[16] = 3;
  b.red[18] = 5;
  size_t size = 1024;
  char buf[1024];
  char* s0 = buf;
  getline(&s0, &size, stdin);
  Color our_color;
  if (!memcmp(s0, "white", 5)) {
    our_color = WHITE;
  } else if (!memcmp(s0, "red", 3)) {
    our_color = RED;
  } else {
    assert(0);
  }
  //fprintf(log_file, "%s", s0);
  //fprintf(log_file, "302 i: %d\n", 0);
  getline(&s0, &size, stdin);
  //fprintf(log_file, "%s", s0);
  //fprintf(log_file, "305 i: %d\n", 0);
  u8 die0;
  u8 die1;
  parse_dice(s0, &die0, &die1);
  Color c;
  if (die0 > die1) {
    c = WHITE;
  } else {
    c = RED;
  }
  //fprintf(log_file, "316 i: %d\n", 0);
  u8 from[BOARD_SIZE];
  u8 to[BOARD_SIZE];
  while (1) {
    if (c == our_color) {
      if (die0 == die1) {
        for (int i = 0; i < 4; i++) {
          u8 moves = generate_moves(from, to, &b, our_color, die0);
          if (moves == 0) {
            break;
          }
          u8 j = random_number() % moves;
          Move move = (Move){from[j], to[j]};
          send_move(move);
          apply_move(&b, c, move.from, move.to);
        }
      } else {
        if (die0 < die1) {
          u8 tmp = die0;
          die0 = die1;
          die1 = tmp;
        }
        for(int i = 0; i < 2; i++) {
          u8 moves = generate_moves(from, to, &b, our_color, die0);
          if (moves == 0) {
            die0 = die1;
            continue;
          }
          u8 j = random_number() % moves;
          Move move = (Move){from[j], to[j]};
          send_move(move);
          apply_move(&b, c, move.from, move.to);
        }
      }
      //fprintf(log_file, "ending_turn\n");
      printf("e\n");
    } else {
      while (1) {
        getline(&s0, &size, stdin);
        //fprintf(log_file, "receiving move\n");
        //fprintf(log_file, "move: %s\n", s0);
        if (s0[0] == 'e') {
          break;
        }
        assert(s0[0] == 'm');
        char* s1;
        u8 f = strtol(s0 + 1, &s1, 10);
        u8 t = strtol(s1, NULL, 10);
        apply_move(&b, c, f, t);
      }
    }
    c = invert(c);
    getline(&s0, &size, stdin);
    parse_dice(s0, &die0, &die1);
    //fprintf(log_file, "end turn\ndice: %d %d\n", die0, die1);
  }
}
