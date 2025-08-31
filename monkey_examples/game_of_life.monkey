############################################################
# Conway's Game of Life — Monkey implementation (finite grid)
# - Live cell: 1
# - Dead cell: 0
# - Out-of-bounds neighbors count as 0 (no wraparound)
############################################################

# --- Grid utilities ---
let rows = fn(g) { len(g) };

let cols = fn(g) {
    # assume first element exists
    len(first(g))
};

# Safe cell access: returns 0 when (r,c) is out of bounds
let cell = fn(g, r, c) {
    if (r < 0) { 0 } 
    else {
        if (r >= rows(g)) { 0 } 
        else {
            let row = g[r];
            if (c < 0) { 0 } 
            else {
                if (c >= cols(g)) { 0 } else {row[c]}
            }
        }
    }
};

# Count the 8 neighbors around (r, c)
let neighbor_count = fn(g, r, c) {
    cell(g, r-1, c-1) +
    cell(g, r-1, c  ) +
    cell(g, r-1, c+1) +
    cell(g, r  , c-1) +
    cell(g, r  , c+1) +
    cell(g, r+1, c-1) +
    cell(g, r+1, c  ) +
    cell(g, r+1, c+1)
};

# Game of Life rule for a single cell
let next_cell = fn(g, r, c) {
    let alive = cell(g, r, c);
    let n = neighbor_count(g, r, c);

    if (alive == 1) {
        # Survival on 2 or 3 neighbors
        if (n == 2) { 1 } else {
        if (n == 3) { 1 } else { 0 }}
    } else {
        # Birth on exactly 3 neighbors
        if (n == 3) { 1 } else { 0 }
    }
};

# Build next row r as a new array
let evolve_row = fn(g, r) {
    let c = 0;
    let C = cols(g);
    let acc = [];
    while (c < C) {
        let acc = push(acc, next_cell(g, r, c));
        let c = c + 1;
    };
    acc
};

# Build next generation grid as a new array of arrays
let evolve = fn(g) {
    let r = 0;
    let R = rows(g);
    let acc = [];
    while (r < R) {
        let acc = push(acc, evolve_row(g, r));
        let r = r + 1;
    };
    acc
};

# --- Pretty printer for the grid ---
let row_to_string = fn(row) {
    let i = 0;
    let s = "";
    let n = len(row);
    while (i < n) {
        let ch = if (row[i] == 1) { "#" } else { "." };
        let s = s + ch;
        let i = i + 1;
    };
    s
};

let print_grid = fn(g) {
    let r = 0;
    let R = rows(g);
    while (r < R) {
        puts(row_to_string(g[r]));
        let r = r + 1;
    };
};

############################################################
# Demo: run a few generations of a glider on a 10x10 board
############################################################

# Place a small "glider" in the top-left
let seed = [
    [0,1,0,0,0,0,0,0,0,0],
    [0,0,1,0,0,0,0,0,0,0],
    [1,1,1,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,0]
];

# Run N generations, printing each
let run = fn(g, gens) {
    let i = 0;
    while (i < gens+1) {
        puts("Generation " + i);
        print_grid(g);
        puts("");  # blank line
        let g = evolve(g);
        let i = i + 1;
    };
};

# --- Execute demo (change gens to taste) ---
puts("starting");
run(seed, 31);