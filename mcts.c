#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define i8  int8_t
#define u8  uint8_t
#define u32 uint32_t
#define u64 uint64_t

#define POOL_ALLOCATOR_CHUNK_SIZE 30000

#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define BAR_INDEX 255
#define QUADRANT_SIZE 6
#define BOARD_SIZE (4 * QUADRANT_SIZE)
#define NUM_CHECKERS 15
#define NUM_DICE 2

FILE* log_file;

//#define LOG(...)
#define LOG(...) fprintf(log_file, __VA_ARGS__)

typedef enum {
  RED,
  WHITE,
} Color;

//White is positive
//Black is negative
//0-23 are the points
//24 is the white bar
//25 is the black bar
//26 is the white home
//27 is the black home
typedef struct {
  i8 board[28];
} Board;

void log_board(Board* b) {
  int white = 0;
  int red = 0;
  for(int i = 0; i < 28; i++) {
    i8 x = b->board[i];
    if (x > 0) {
      white += x;
    } else {
      red -= x;
    }
  }
  for(int i = 0; i < 28; i++) {
    LOG("%*d", 3, b->board[i]);
  }
  LOG("\n");
}

u64 xorshift_state = 8425289453374772393;

u32 random_number() {
  u64 x = xorshift_state;
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  xorshift_state = x;
  return (u32)((x * UINT64_C(2685821657736338717)) >> 32);
}

void seed_prng(void) {
  struct timespec t;
  for(int i = 0; i < 4; i++) {
    assert(!clock_gettime(CLOCK_MONOTONIC, &t));
    xorshift_state ^= (u64)(t.tv_nsec << 32);
    xorshift_state ^= (u64)(t.tv_sec);
    xorshift_state *= random_number();
  }
}

typedef struct Pool_Allocator Pool_Allocator;

struct Pool_Allocator {
  Pool_Allocator* prev;
  void* start;
  void* current;
  void* end;
};

Pool_Allocator pool_allocator() {
  Pool_Allocator a;
  a.prev = NULL;
  a.start = malloc(POOL_ALLOCATOR_CHUNK_SIZE);
  a.current = a.start;
  a.end = a.start + POOL_ALLOCATOR_CHUNK_SIZE - sizeof(Pool_Allocator);
  return a;
}

void* allocate_from_pool(Pool_Allocator* a, size_t size) {
  //return calloc(size, 1); //TODO: fixme.
  assert(size < POOL_ALLOCATOR_CHUNK_SIZE);
  void* current = a->current;
  if (current + size >= a->end) {
    Pool_Allocator b = pool_allocator();
    Pool_Allocator* a_ptr = allocate_from_pool(&b, sizeof(Pool_Allocator));
    *a_ptr = *a;
    b.prev = a_ptr;
    *a = b;
    return allocate_from_pool(a, size);
  }
  a->current += size;
  return current;
}

void delete_pool(Pool_Allocator a) {
  while (a.prev != NULL) {
    Pool_Allocator b = *(a.prev);
    free(a.start);
    a = b;
  }
}
u8 get_index(Color c, u8 pos) {
  if (c == WHITE) {
    return pos;
  }
  return 23 - pos;
}

int generate_moves(u8* moves, Board* b, Color c, u8 die) {
  assert(1 <= die && die <= 6);
  u8 move_index = 0;
  u8 bar_index;
  u8 home_index;
  i8 sign;
  if (c == WHITE) {
    sign = 1;
    bar_index = 24;
    home_index = 26;
  } else {
    sign = -1;
    bar_index = 25;
    home_index = 27;
  }
  if (sign * b->board[home_index] == 15) {
    return 255;
  }
  if (b->board[bar_index]) {
    u8 from = get_index(c, -1 + (int)die);
    i8 height = sign * b->board[from];
    if (height >= -1) {
      moves[0] = bar_index;
      moves[1] = from;
      return 1;
    }
    return 0;
  }
  int min;
  for(min = 0; (min < 24) && (sign * b->board[get_index(c, min)] <= 0); min += 1);
  int i;
  for(i = min; i < 24 - die; i++) {
    u8 from = get_index(c, i);
    if (sign * b->board[from] > 0) {
      u8 to = from + sign * die;
      i8 height = sign * b->board[to];
      if (height >= -1) {
        assert(move_index < 2 * 24);
        moves[move_index] = from;
        moves[move_index + 1] = to;
        move_index += 2;
      }
    }
  }
  bool bearing_off = min >= 3 * 6;
  if (bearing_off) {
    for(; i < 24; i++) {
      u8 from = get_index(c, i);
      if (sign * b->board[from] > 0) {
        u8 j = i + die;
        if ((j == 24) || ((j > 24) && (i == min))) {
          assert(move_index < 2 * 24);
          moves[move_index] = from;
          moves[move_index + 1] = home_index;
          move_index += 2;
          break;
        }
      }
    }
  }
  return move_index >> 1;
}

//White is positive
//Black is negative
//0-23 are the points
//24 is the white bar
//25 is the black bar
//26 is the white home
//27 is the black home

void apply_move(Board* b, Color c, u8 from, u8 to) {
  assert(from != to);
  i8 sign;
  u8 bar_index;
  u8 opponent_bar_index;
  u8 home_index;
  if (c == WHITE) {
    sign = 1;
    bar_index = 24;
    opponent_bar_index = 25;
    home_index = 26;
  } else {
    sign = -1;
    bar_index = 25;
    opponent_bar_index = 24;
    home_index = 27;
  }
  if (from >= 24) {
    assert(from == bar_index);
  }
  if (to >= 24) {
    assert(to == home_index);
  }
  assert(sign * b->board[from] >= 1);
  b->board[from] -= sign;
  assert(sign * b->board[to] >= -1);
  if (b->board[to] == (-sign)) {
    b->board[to] = sign;
    b->board[opponent_bar_index] -= sign;
  } else {
    b->board[to] += sign;
  }
}

Color invert(Color c) {
  if (c == WHITE) {
    return RED;
  }
  return WHITE;
}

u8 roll_die() {
  return (u8)(random_number() % 6 + 1);
}

Color playout(Board* b, Color c) {
  u8 moves[2 * 24];
  int x = 0;
  while (1) {
    if (c == WHITE) {
    } else {
    }
    u8 die0 = roll_die();
    u8 die1 = roll_die();
    if (die0 < die1) {
      u8 tmp = die0;
      die0 = die1;
      die1 = tmp;
    }
    if (die0 == die1) {
      for(int i = 0; i < 4; i++) {
        u8 num_moves = generate_moves(moves, b, c, die0);
        if (num_moves == 255) {
          return c;
        }
        if (num_moves == 0) {
          break;
        }
        int j = random_number() % num_moves;
        apply_move(b, c, moves[2 * j], moves[2 * j + 1]);
      }
    } else {
      int passes = 0;
      for(int i = 0; i < 3; i++) {
        u8 num_moves = generate_moves(moves, b, c, die0);
        if (num_moves == 255) {
          return c;
        } else if (num_moves == 0) {
          passes += 1;
        } else {
          int j = random_number() % num_moves;
          apply_move(b, c, moves[2 * j], moves[2 * j + 1]);
        }
        if (i == 1 && passes != 1) {
          break;
        }
        u8 tmp = die0;
        die1 = die0;
        die0 = tmp;
      }
    }
    c = invert(c);
  }
}

u64 get_time_in_usecs(void) {
  struct timespec t;
  assert(!clock_gettime(CLOCK_MONOTONIC, &t));
  return (u64)(t.tv_sec) * 1000000 + (u64)(t.tv_nsec) / 1000;
}

typedef enum {
  RANDOM,
  MOVE,
} NodeType;

typedef struct Node Node;

struct Node {
  NodeType type;
  float score;
  Color color;
  u32 playouts;
  Node* children;
  u8 num_children;
  u8 num_dice;
  u8 die;
  u8 from;
  u8 to;
};

typedef struct {
  Board board;
  u8    dice[4];
  u8    num_dice;
} GameState;

void update(Node* node, Color c) {
  int playouts = node->playouts + 1;
  float fplayouts = (float)(playouts);
  float k = (float)(node->playouts) / fplayouts;
  float result;
  if (c == node->color) {
    result = 1;
  } else {
    result = -1;
  }
  node->score = k * node->score + result / fplayouts;
  node->playouts = playouts;
}

int DEPTH;

Color select_random(Pool_Allocator*, Node*, GameState);

Color select_move(Pool_Allocator* allocator, Node* node, GameState state) {
  DEPTH += 1;
  assert(node->type == MOVE);
  if (node->playouts == 0) {
    u8 moves[2 * 24];
    assert(state.num_dice != 0);
    assert(state.num_dice <= 4);
    u8 unique_dice;
    if ((state.num_dice == 2) && (state.dice[0] != state.dice[1])) {
      unique_dice = 2;
    } else {
      unique_dice = 1;
    }
    for (int i = 0; i < unique_dice; i++) {
      u8 die = state.dice[state.num_dice - 1 - i];
      u8 num_moves = generate_moves(moves, &(state.board), node->color, die);
      if (num_moves == 255) {
        update(node, node->color);
        return node->color;
      } else if (num_moves != 0) {
        node->die = die;
        node->children = allocate_from_pool(allocator, num_moves * sizeof(Node));
        node->num_children = num_moves;
        memset(node->children, 0, num_moves * sizeof(Node));
        if (state.num_dice == 1) {
          for (int i = 0; i < num_moves; i++) {
            Node* child = node->children + i;
            child->type = RANDOM;
            child->color = invert(node->color);
            child->from = moves[2 * i];
            child->to = moves[2 * i + 1];
          }
        } else {
          for (int i = 0; i < num_moves; i++) {
            Node* child = node->children + i;
            child->type = MOVE;
            child->from = moves[2 * i];
            child->to = moves[2 * i + 1];
            child->color = node->color;
          }
        }
        node->die = die;
        Board b = state.board;
        Color c = playout(&b, node->color);
        update(node, c);
        return c;
      }
    }
    node->type = RANDOM;
    state.num_dice = 0;
    node->color = invert(node->color);
    return select_random(allocator, node, state);
  }
  if (node->children == 0) {
    assert(true); //TODO: check winner.
    update(node, node->color);
    return node->color;
  }
  float n = (float)node->playouts;
  int best_index = 0;
  float best_ucb = 0;
  for(int i = 0; i < node->num_children; i++) {
    Node* child = node->children + i;
    float si = (float)(child->score);
    float ni = (float)(child->playouts);
    if (ni == 0) {
      best_index = i;
      break;
    }
    float ucb = si + sqrt(2 * log(n) / ni);
    if (ucb > best_ucb) {
      best_ucb = ucb;
      best_index = i;
    }
  }
  Node* child = node->children + best_index;
  assert(child->from != child->to);
  apply_move(&(state.board), node->color, child->from, child->to);
  state.num_dice -= 1;
  for(int i = 0; i < state.num_dice; i++) {
    if (state.dice[i] == node->die) {
      for(; i < state.num_dice; i++) {
        state.dice[i] = state.dice[i + 1];
      }
      break;
    }
  }
  Color c;
  if (child->type == MOVE) {
    c = select_move(allocator, child, state);
  } else {
    state.num_dice = 0;
    c = select_random(allocator, child, state);
  }
  update(node, c);
  return c;
}

Color select_random(Pool_Allocator* allocator, Node* node, GameState state) {
  DEPTH += 1;
  assert(node->type == RANDOM);
  if (node->playouts == 0) {
    node->num_dice = state.num_dice;
    node->children = allocate_from_pool(allocator, 6 * sizeof(Node));
    assert(node->children != NULL);
    node->num_children = 6;
    memset(node->children, 0, 6 * sizeof(Node));
    assert(node->children != NULL);
    for(int i = 0; i < 6; i++) {
      Node* child = node->children + i;
      child->playouts = 0;
      child->die = i;
      child->color = node->color;
      if (state.num_dice == 0) {
        child->type = RANDOM;
      } else {
        child->type = MOVE;
      }
    }
    Color c = playout(&(state.board), node->color);
    update(node, c);
    return c;
  }
  u8 die = roll_die();
  assert(state.num_dice == node->num_dice);
  assert(state.num_dice < 2);
  state.dice[state.num_dice++] = die;
  if (state.num_dice == 2) {
    if (state.dice[0] == die) {
      state.dice[2] = state.dice[0];
      state.dice[3] = state.dice[0];
      state.num_dice = 4;
    } else {
      assert(state.num_dice <= 2);
      if (state.dice[0] < state.dice[1]) {
        u8 tmp = state.dice[0];
        state.dice[0] = state.dice[1];
        state.dice[1] = tmp;
      }
    }
  }
  Node* child = node->children + die - 1;
  Color c;
  if (child->type == MOVE) {
    c = select_move(allocator, child, state);
  } else {
    if (child->color != node->color) {
      state.num_dice = 0;
    }
    c = select_random(allocator, child, state);
  }
  update(node, c);
  return c;
}

void mcts_search(Pool_Allocator* allocator, GameState state, Node* root, u32 time_limit) {
  u64 t0 = get_time_in_usecs();
  int playout_block = 8;
  u64 t1 = t0;
  int j = 0;
  do {
    j += 1;
    for(int i = 0; i < playout_block; i++) {
      DEPTH = 0;
      if (root->type != MOVE) {
        return;
      }
      select_move(allocator, root, state);
      //log_board(&state.board);
    }
    u64 t2 = get_time_in_usecs();
    u64 dt = t2 - t1;
    t1 = t2;
    u64 rate = (256 * playout_block) / dt; // Prescale to increase resolution.
    u64 time_remaining = t0 + time_limit - t2;
    playout_block = (rate * time_remaining) / (2 * 256);
  } while (playout_block > 1);
}

void parse_dice(char* s0, u8* die0, u8* die1) {
  assert(s0[0] == 'd');
  char* s1;
  *die0 = strtol(s0 + 1, &s1, 10);
  *die1 = strtol(s1, NULL, 10);
  return;
}

typedef struct {
  u8 from;
  u8 to;
} Move;

void send_move(Color c, Move m) {
  if (c == WHITE) {
    if (m.from >= 24) {
      m.from = -1;
    }
    if (m.to >= 24) {
      m.to = 24;
    }
    printf("m %d %d\n", m.from, m.to);
  } else {
    if (m.from >= 24) {
      m.from = 23 - (-1);
    }
    if (m.to >= 24) {
      m.to = 23 - 24;
    }
    printf("m %d %d\n", 23 - m.from, 23 - m.to);
  }
}

int main(void) {
  seed_prng();
  char log_file_name[1024];
  sprintf((char*)(log_file_name), "log%lu.txt", (u64)time(NULL));
  log_file = fopen(log_file_name, "w+");
  setvbuf(log_file, NULL, _IOLBF, 1024);
  setvbuf(stdout, NULL, _IOLBF, 1024);
  setvbuf(stdin, NULL, _IOLBF, 1024);
  Board b = {0};
  u8 indices[4] = {0, 11, 16, 18};
  u8 heights[4] = {2, 5, 3, 5};
  for(int i = 0; i < 4; i++) {
    u8 j = indices[i];
    u8 h = heights[i];
    b.board[j] = h;
    b.board[23 - j] = -h;
  }
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
  getline(&s0, &size, stdin);
  u8 die0;
  u8 die1;
  parse_dice(s0, &die0, &die1);
  Color c;
  if (die0 > die1) {
    c = WHITE;
  } else {
    c = RED;
  }
  while (1) {
    //log_board(&b);
    if (c == our_color) {
      GameState state;
      state.board = b;
      if (die0 == die1) {
        state.num_dice = 4;
        for (int i = 0; i < 4; i++) {
          state.dice[i] = die0;
        }
      } else {
        state.num_dice = 2;
        state.dice[0] = die0;
        state.dice[1] = die1;
      }
      Node root = {0};
      root.type = MOVE;
      root.color = c;
      Pool_Allocator allocator = pool_allocator();
      //log_board(&b);
      mcts_search(&allocator, state, &root, 300000);
      //log_board(&b);
      Node node = root;
      while (node.type == MOVE) {
        if (node.playouts == 0) {
          select_move(&allocator, &node, state);
        }
        assert(node.color == c);
        int best_index = -1;
        int highest_playouts = 0;
        for (int i = 0; i < node.num_children; i++) {
          u32 playouts = node.children[i].playouts;
          if (playouts >= highest_playouts) {
            best_index = i;
            highest_playouts = playouts;
          }
        }
        if (best_index == -1) {
          if (node.num_children == 0) {
            break;
          } else {
            best_index = 0;
          }
        }
        node = node.children[best_index];
        state.num_dice -= 1;
        for(int i = 0; i < state.num_dice; i++) {
          if (state.dice[i] == node.die) {
            for(; i < state.num_dice; i++) {
              state.dice[i] = state.dice[i + 1];
            }
            break;
          }
        }
        Move move;
        move.from = node.from;
        move.to = node.to;
        send_move(c, move);
        apply_move(&(state.board), c, node.from, node.to);
      };
      b = state.board;
      //fprintf(log_file, "ending_turn\n");
      printf("e\n");
      delete_pool(allocator);
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
        u8 f0 = strtol(s0 + 1, &s1, 10);
        u8 t0 = strtol(s1, NULL, 10);
        u8 f;
        u8 t;
        //log_board(&b);
        if (f0 == 255) {
          if (c == WHITE) {
            f = 24;
          } else {
            f = 25;
          }
        } else {
          f = get_index(c, f0);
        }
        if (t0 == 24) {
          if (c == WHITE) {
            t = 26;
          } else {
            t = 27;
          }
        } else {
          t = get_index(c, t0);
        }
        apply_move(&b, c, f, t);
      }
    }
    c = invert(c);
    getline(&s0, &size, stdin);
    parse_dice(s0, &die0, &die1);
    //fprintf(log_file, "end turn\ndice: %d %d\n", die0, die1);
  }
}
