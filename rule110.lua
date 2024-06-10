-- Define the Rule 110 lookup table
local rule110 = {
    ["111"] = "0",
    ["110"] = "1",
    ["101"] = "1",
    ["100"] = "0",
    ["011"] = "1",
    ["010"] = "1",
    ["001"] = "1",
    ["000"] = "0"
}

-- Function to apply Rule 110 to a given state
local function applyRule110(state)
    local newState = ""
    local length = #state

    for i = 1, length do
        -- Get the current triplet (wrapping around at the edges)
        local left = state:sub((i - 2) % length + 1, (i - 2) % length + 1)
        local center = state:sub(i, i)
        local right = state:sub(i % length + 1, i % length + 1)

        local triplet = left .. center .. right

        -- Determine the new state of the center cell
        newState = newState .. rule110[triplet]
    end

    return newState
end

-- Function to run the simulation
local function runSimulation(initialState, steps)
    local state = initialState

    for i = 1, steps do
        print(state)
        state = applyRule110(state)
    end
end

-- Define the initial state and number of steps
local initialState = ("0"):rep(99) .. "1"
local steps = 100

-- Run the simulation
runSimulation(initialState, steps)
