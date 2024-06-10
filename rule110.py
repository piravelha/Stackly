def apply_rule110(state):
    # Define the Rule 110 lookup table
    rule110 = {
        (1, 1, 1): 0,
        (1, 1, 0): 1,
        (1, 0, 1): 1,
        (1, 0, 0): 0,
        (0, 1, 1): 1,
        (0, 1, 0): 1,
        (0, 0, 1): 1,
        (0, 0, 0): 0
    }
    
    # Create the new state
    new_state = []
    length = len(state)
    
    for i in range(length):
        # Get the current triplet (wrapping around at the edges)
        left = state[(i - 1) % length]
        center = state[i]
        right = state[(i + 1) % length]
        
        # Determine the new state of the center cell
        new_state.append(rule110[(left, center, right)])
    
    return new_state

def print_state(state):
    # Convert the state to a string for easy reading
    print(''.join('*' if cell == 1 else ' ' for cell in state))

def run_simulation(initial_state, steps):
    state = initial_state
    
    for _ in range(steps):
        print_state(state)
        state = apply_rule110(state)

# Define the initial state and number of steps
initial_state = [0] * 99 + [1]
steps = 100

# Run the simulation
run_simulation(initial_state, steps)
