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
#define i64 int64_t
#define u8  uint8_t
#define u32 uint32_t
#define u64 uint64_t

#define NDEBUG

#ifdef NDEBUG
#define LOG(...)
#else
FILE* log_file;
#define LOG(...) fprintf(log_file, __VA_ARGS__)
#endif

u64 pcg_state;

u32 random_number() {
    u64 oldstate = pcg_state;
    pcg_state = oldstate * 6364136223846793005ULL + 9067608870314829635ULL;
    u32 xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
    u32 rot = oldstate >> 59u;
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

void seed_prng(void) {
  pcg_state = 10199641363321847927ULL;
  struct timespec t;
  for(int i = 0; i < 40; i++) {
    int err = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);
    if (err) {
      printf("error: seed_prng: clock_gettime");
      exit(EXIT_FAILURE);
    };
    pcg_state ^= ((u64)(t.tv_nsec)) << 32;
    pcg_state ^= t.tv_sec;
    pcg_state += random_number();
    pcg_state ^= random_number();
  }
  for(int i = 0; i < 8; i++) {
    pcg_state *= random_number();
    pcg_state ^= random_number();
  }
}

typedef struct Pool_Allocator Pool_Allocator;

struct Pool_Allocator {
  char* start;
  char* current;
  char* end;
};

Pool_Allocator pool_allocator(size_t size) {
  Pool_Allocator a;
  a.start = malloc(size);
  if (!a.start) {
    printf("error: out of memory");
    exit(EXIT_FAILURE);
  }
  a.current = a.start;
  assert(a.start <= a.end - size);
  a.end = a.start + size;
  return a;
}

void* allocate_from_pool(Pool_Allocator* a, size_t size) {
  char* current = a->current;
  assert(current < a->end);
  assert((uintptr_t)current <= UINTPTR_MAX - size);
  if (current + size >= a->end) {
    return NULL;
  }
  a->current += size;
  return current;
}

void delete_pool(Pool_Allocator a) {
  free(a.start);
}

typedef enum {
  RED = 0,
  WHITE = 1,
} Color;

i8 sign(Color c) {
  if (c == WHITE) {
    return 1;
  }
  return -1;
}

Color invert(Color c) {
  if (c == WHITE) {
    return RED;
  }
  return WHITE;
}

#define WHITE_BAR 24
#define RED_BAR 25
#define WHITE_HOME 26
#define RED_HOME 27

typedef struct {
  i8 board[28];
} Board;

i8* bar(Board* b, Color c) {
  if (c == WHITE) {
    return b->board + WHITE_BAR;
  }
  return b->board + RED_BAR;
}

i8* home(Board* b, Color c) {
  if (c == WHITE) {
    return b->board + WHITE_HOME;
  }
  return b->board + RED_HOME;
}

u8 get_index(Color c, u8 point) {
  if (c == WHITE) {
    return point;
  }
  return 23 - point;
}

#define WON 255

int generate_moves(u8* moves, Board* b, Color c, u8 die) {
  assert(1 <= die && die <= 6);
  u8 home_index;
  u8 bar_index;
  if (c == WHITE) {
    home_index = WHITE_HOME;
    bar_index  = WHITE_BAR;
  } else {
    home_index = RED_HOME;
    bar_index  = RED_BAR;
  }
  u8 move_index = 0;
  i8 s = sign(c);
  if (s * (*home(b, c)) == 15) {
    return WON;
  }
  if (*bar(b, c)) {
    u8 from = get_index(c, -1 + (int)die);
    i8 height = s * b->board[from];
    if (height >= -1) {
      moves[0] = bar_index;
      moves[1] = from;
      return 1;
    }
    return 0;
  }
  int min;
  if (c == WHITE) {
    for(min = 0; (min < 24) && (b->board[min] <= 0); min += 1);
  } else {
    for(min = 24; (min >= 0) && (b->board[min] >= 0); min -= 1);
    min = 23 - min;
  }
  int i;
  for(i = min; i < 24 - die; i++) {
    u8 from = get_index(c, i);
    if (s * b->board[from] > 0) {
      u8 to = get_index(c, i + die);
      i8 height = s * b->board[to];
      if (height >= -1) {
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
      if (s * b->board[from] > 0) {
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
  return move_index / 2;
}

void apply_move(Board* b, Color c, u8 from, u8 to) {
  assert(from != to);
  i8 s = sign(c);
  u8 bar_index;
  u8 opponent_bar_index;
  u8 home_index;
  if (c == WHITE) {
    bar_index = WHITE_BAR;
    opponent_bar_index = RED_BAR;
    home_index = WHITE_HOME;
  } else {
    bar_index = RED_BAR;
    opponent_bar_index = WHITE_BAR;
    home_index = RED_HOME;
  }
  if (from >= 24) {
    assert(from == bar_index);
  }
  if (to >= 24) {
    assert(to == home_index);
  }
  assert(s * b->board[from] >= 1);
  b->board[from] -= s;
  assert(s * b->board[to] >= -1);
  if (b->board[to] == (-s)) {
    b->board[to] = s;
    b->board[opponent_bar_index] -= s;
  } else {
    b->board[to] += s;
  }
}

u8 roll_die() {
  return (u8)(random_number() % 6 + 1);
}

Color playout(Board* b, Color c, u8 die0, u8 die1, u8 num_dice) {
  u8 moves[2 * 24];
  if (num_dice <= 1) {
    assert(num_dice == 1);
    u8 num_moves = generate_moves(moves, b, c, die0);
    if (num_moves == 255) {
      assert(abs(*home(b, c)) == 15);
      return c;
    }
    if (num_moves != 0) {
      int j = random_number() % num_moves;
      apply_move(b, c, moves[2 * j], moves[2 * j + 1]);
    }
    c = invert(c);
    die0 = roll_die();
    die1 = roll_die();
    if (die0 < die1) {
      u8 tmp = die0;
      die0 = die1;
      die1 = tmp;
    }
  }
  while (1) {
    if (die0 == die1) {
      for(int i = 0; i < num_dice; i++) {
        u8 num_moves = generate_moves(moves, b, c, die0);
        if (num_moves == 255) {
          assert(abs(*home(b, c)) == 15);
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
          assert(abs(*home(b, c)) == 15);
          //LOG2("%d\n", c);
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
    die0 = roll_die();
    die1 = roll_die();
    if (die0 < die1) {
      u8 tmp = die0;
      die0 = die1;
      die1 = tmp;
    } else if (die0 == die1) {
      num_dice = 4;
    }
  }
}

u64 get_time_in_usecs(void) {
  struct timespec t;
  int result = clock_gettime(CLOCK_MONOTONIC, &t);
  assert(result == 0);
  return (u64)(t.tv_sec) * (1000 * 1000) + (u64)(t.tv_nsec) / 1000;
}

typedef enum {
  RANDOM,
  MOVE,
} NodeType;

typedef struct Node Node;

struct Node {
  NodeType type;
  double score;
  Color color;
  u32 playouts;
  Node* children;
  u8 num_children;
  u8 num_dice;
  u8 die;
  u8 from;
  u8 to;
  u8 index;
};

void log_node(Node* node) {
  LOG("{\"type\": %d, ", node->type);
  LOG("\"score\": %f, ", node->score);
  LOG("\"color\": %d, ", node->color);
  LOG("\"playouts\": %d, ", node->playouts);
  LOG("\"num_children\": %d, ", node->num_children);
  LOG("\"num_dice\": %d, ", node->num_dice);
  LOG("\"die\": %d, ", node->die);
  LOG("\"from\": %d, ", node->from);
  LOG("\"to\": %d, ", node->to);
  LOG("\"children\": [");
  for(int i = 0; i < node->num_children; i++) {
    log_node(node->children + i);
    if (i != node->num_children - 1) {
      LOG(", ");
    }
  }
  LOG("]}\n");
}

typedef struct {
  Board board;
  u8    dice[2];
  u8    num_dice;
} GameState;

void update(Node* node, Color c) {
  //LOG("old_score: %f\n", node->score);
  //LOG("old_playouts: %d\n", node->playouts);
  int playouts = node->playouts + 1;
  double fplayouts = (double)(playouts);
  double k = (double)(node->playouts) / fplayouts;
  double result;
  if (c == node->color) {
    result = 1;
  } else {
    result = -1;
  }
  //LOG("k: %f\n", k);
  //LOG("result: %f\n", result);
  node->score = k * node->score + result / fplayouts;
  node->playouts = playouts;
  //LOG("playouts: %d\n", node->playouts);
  //LOG("new_score: %f\n", node->score);
}

typedef enum {
  RED_S = 0,
  WHITE_S = 1,
  OUT_OF_MEMORY_S = 2,
} Selection;

Selection select_random(Pool_Allocator*, Node*, GameState);

Selection select_move(Pool_Allocator* allocator, Node* node, GameState state) {
  LOG("move\n");
  LOG("color: %d\n", node->color);
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
      u8 die = state.dice[i];
      node->die = die;
      u8 num_moves = generate_moves(moves, &(state.board), node->color, die);
      if (num_moves == 255) {
        assert(abs(*home(&(state.board), node->color)) == 15);
        update(node, node->color);
        return (Selection)node->color;
      } else if (num_moves != 0) {
        node->children = allocate_from_pool(allocator, num_moves * sizeof(Node));
        if (node->children == NULL) {
          return OUT_OF_MEMORY_S;
        }
        node->num_children = num_moves;
        memset(node->children, 0, num_moves * sizeof(Node));
        if (state.num_dice == 1) {
          for (int j = 0; j < num_moves; j++) {
            Node* child = node->children + j;
            child->type = RANDOM;
            child->color = node->color;
            child->from = moves[2 * j];
            child->to = moves[2 * j + 1];
          }
        } else {
          for (int j = 0; j < num_moves; j++) {
            Node* child = node->children + j;
            child->type = MOVE;
            child->from = moves[2 * j];
            child->to = moves[2 * j + 1];
            child->color = node->color;
          }
        }
        node->die = die;
        Color c = playout(&state.board, node->color,
                                        state.dice[0],
                                        state.dice[1],
                                        state.num_dice);
        update(node, c);
        return (Selection)c;
      }
    }
    node->type = RANDOM;
    state.num_dice = 0;
    node->index = -1;
    return select_random(allocator, node, state);
  }
  if (node->children == 0) {
    update(node, node->color);
    return (Selection)(node->color);
  }
  double n = (double)node->playouts;
  int best_index = 0;
  double best_ucb = -INFINITY;
  for(int i = 0; i < node->num_children; i++) {
    Node* child = node->children + i;
    double si = (double)(child->score);
    double ni = (double)(child->playouts);
    if (ni == 0) {
      best_index = i;
      break;
    }
    double ucb = si + sqrt(2 * log(n) / ni);
    if (ucb > best_ucb) {
      best_ucb = ucb;
      best_index = i;
    }
  }
  Node* child = node->children + best_index;
  assert(child->from != child->to);
  apply_move(&(state.board), node->color, child->from, child->to);
  Selection s;
  if (child->type == MOVE) {
    if (state.num_dice == 2) {
      if (state.dice[0] == node->die) {
        state.dice[0] = state.dice[1];
      } else {
        assert(state.dice[1] == node->die);
      }
    } else {
      assert(state.dice[0] == node->die);
    }
    state.num_dice -= 1;
    s = select_move(allocator, child, state);
  } else {
    state.num_dice = 0;
    s = select_random(allocator, child, state);
  }
  if (s != OUT_OF_MEMORY_S) {
    update(node, (Color)s);
  }
  return s;
}

Selection select_random(Pool_Allocator* allocator, Node* node, GameState state) {
  LOG("chance\n");
  assert(node->type == RANDOM);
  if (node->playouts == 0) {
    node->children = allocate_from_pool(allocator, 6 * sizeof(Node));
    if (node->children == NULL) {
      return OUT_OF_MEMORY_S;
    }
    memset(node->children, 0, 6 * sizeof(Node));
    node->num_dice = state.num_dice;
    node->num_children = 6;
    for(int i = 0; i < 6; i++) {
      Node* child = node->children + i;
      child->playouts = 0;
      child->die = i + 1;
      if (state.num_dice == 0) {
        child->type = RANDOM;
        child->color = node->color;
      } else {
        child->type = MOVE;
        child->color = invert(node->color);
      }
    }
    while (state.num_dice < 2) {
      state.dice[(state.num_dice)++] = roll_die();
    }
    Color c = playout(&(state.board), node->color, state.dice[0], state.dice[1], state.num_dice);
    update(node, c);
    return (Selection)c;
  }
  u8 child_index = random_number() % 6;
  Node* child = node->children + child_index;
  u8 die = child_index + 1;
  LOG("state: %u, node: %d, index: %d, color: %d\n", state.num_dice, node->num_dice, node->index, node->color);
  assert(state.num_dice == node->num_dice);
  assert(state.num_dice < 2);
  state.dice[state.num_dice++] = die;
  if (state.num_dice == 2) {
    if (state.dice[0] == die) {
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
  Selection s;
  if (child->type == MOVE) {
    s = select_move(allocator, child, state);
  } else {
    if (child->color != node->color) {
      state.num_dice = 0;
    }
    s = select_random(allocator, child, state);
  }
  if (s != OUT_OF_MEMORY_S) {
    update(node, (Color)s);
  }
  return s;
}

void mcts_search(Pool_Allocator* allocator, GameState state, Node* root, u32 time_limit) {
  time_limit *= 1000;
  u64 t0 = get_time_in_usecs();
  int playout_block = 8;
  u64 t1 = t0;
  int j = 0;
  do {
    j += 1;
    for(int i = 0; i < playout_block; i++) {
      LOG("-----------------------\n");
      if (root->type != MOVE) {
        return;
      }
      if (select_move(allocator, root, state) == OUT_OF_MEMORY_S) {
        return;
      }
    }
    u64 t2 = get_time_in_usecs();
    u64 dt = t2 - t1;
    t1 = t2;
    i64 time_remaining = t0 + time_limit - t2;
    if (time_remaining < 0) {
      return;
    }
    playout_block = (playout_block * time_remaining) / (2 * dt);
  } while (playout_block > 1);
}

void parse_dice(char* s0, u8* die0, u8* die1) {
  assert(s0[0] == 'd');
  char* s1;
  *die0 = strtol(s0 + 1, &s1, 10);
  *die1 = strtol(s1, NULL, 10);
  return;
}

void send_move(Color c, u8 from, u8 to) {
  if (from >= 24) {
    from = -1;
  } else if (c == RED) {
    from = 23 - from;
  }
  if (to >= 24) {
    to = 24;
  } else if (c == RED) {
    to = 23 - to;
  }
  printf("m %d %d\n", from, to);
}

int main(void) {
  seed_prng();
  #ifndef NDEBUG
  char log_file_name[1024];
  sprintf((char*)(log_file_name), "log%lu.txt", (u64)time(NULL) % 10000000);
  log_file = fopen(log_file_name, "w+");
  setvbuf(log_file, NULL, _IOLBF, 1024);
  #endif
  setvbuf(stdout, NULL, _IOLBF, 1024);
  setvbuf(stdin, NULL, _IOLBF, 1024);
  Board b = {{0}};
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
    if (c == our_color) {
      GameState state;
      state.board = b;
      state.dice[0] = die0;
      state.dice[1] = die1;
      if (die0 == die1) {
        state.num_dice = 4;
      } else {
        state.num_dice = 2;
        if (state.dice[0] < state.dice[1]) {
          u8 tmp = state.dice[0];
          state.dice[0] = state.dice[1];
          state.dice[1] = tmp;
        }
      }
      Node root = {0};
      root.type = MOVE;
      root.color = c;
      Pool_Allocator allocator = pool_allocator(50 * 1000 * 1000);
      Pool_Allocator spare_allocator = pool_allocator(15 * 4 * sizeof(Node));
      mcts_search(&allocator, state, &root, 50);
      //log_node(&root);
      Node node = root;
      while (node.type == MOVE && node.color == c) {
        if (node.playouts == 0) {
          select_move(&spare_allocator, &node, state);
        }
        if (!(node.type == MOVE && node.color == c)) {
          break;
        }
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
          break;
        }
        LOG("num_dice: %d\n", state.num_dice);
        LOG("die: %u\n", node.die);
        LOG("dice: %u %u\n", state.dice[0], state.dice[1]);
        if (state.dice[0] == node.die) {
          state.dice[0] = state.dice[1];
        } else {
          assert(state.dice[1] == node.die);
        }
        state.num_dice -= 1;
        node = node.children[best_index];
        send_move(c, node.from, node.to);
        apply_move(&(state.board), c, node.from, node.to);
      };
      printf("e\n");
      b = state.board;
      delete_pool(allocator);
      delete_pool(spare_allocator);
    } else {
      while (1) {
        getline(&s0, &size, stdin);
        if (s0[0] == 'e') {
          break;
        }
        assert(s0[0] == 'm');
        char* s1;
        u8 f0 = strtol(s0 + 1, &s1, 10);
        u8 t0 = strtol(s1, NULL, 10);
        u8 f;
        u8 t;
        if (f0 == 255) {
          if (c == WHITE) {
            f = WHITE_BAR;
          } else {
            f = RED_BAR;
          }
        } else {
          f = get_index(c, f0);
        }
        if (t0 == 24) {
          if (c == WHITE) {
            t = WHITE_HOME;
          } else {
            t = RED_HOME;
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
