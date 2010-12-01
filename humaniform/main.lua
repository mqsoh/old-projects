-- ===========
-- Humaniform.
-- ===========
-- Author: INKling
--
-- Humaniform makes your charcter act like a human. When active, it will send
-- /me messages to sector chat indicating that you're engaged in human-like
-- activities.
--
-- Usage.
-- ------
-- /humaniform [gender, 'on' or 'off']
--
-- Examples.
-- ---------
-- /humaniform on
-- /humaniform off
-- /humaniform male
-- /humaniform female
-- /humaniform neuter
humaniform = humaniform or {}
humaniform.timer = Timer()

humaniform.genders = { 'male', 'female', 'neuter' }
humaniform.actions = {
    'farts.'
    ,'burps.'
    ,'scratches {posessive} head.'
    ,'picks {posessive} nose.'
    ,'sighs.'
    ,'claps like a maniac!'
    ,'scratches {posessive} butt.'
    ,'strokes {posessive} chin in thought.'
    ,'rubs {posessive} eyes.'
    ,'twitches.'
    ,'takes a sip of {posessive} drink.'
}

humaniform.time_min_s = 5 * 60
humaniform.time_max_s = 20 * 60
humaniform.gender = humaniform.genders[3]
humaniform.active = true

-- =========
-- Schedule.
-- =========
-- Schedules the next event for a random time between time_min and time_max
-- seconds.
function humaniform.schedule()
    local h = humaniform
    local s_from_now = math.random(h.time_min_s, h.time_max_s)
    local ms_from_now = s_from_now * 1000

    h.timer:SetTimeout(ms_from_now, h.tick)
    h.active = true
end

-- =====
-- Tick.
-- =====
-- Executes one of the human actions.
function humaniform.tick()
    local h = humaniform
    local rand = math.random(1, #h.actions)
    local action = h.actions[rand]
    local prepped = string.gsub(action, '{posessive}', h.posessive())

    SendChat('/me '..prepped, 'SECTOR')

    h.schedule()
end

-- ===========
-- Possessive.
-- ===========
-- Returns a posessive pronoun for the configured gender.
function humaniform.posessive()
    local h = humaniform
    if h.gender == 'male' then
        return 'his'
    elseif h.gender == 'female' then
        return 'her'
    else
        return 'its'
    end
end

-- ============
-- Save config.
-- ============
-- Saves the configuration to the system notes.
function humaniform.save_config()
    local h = humaniform
    local id = GetCharacterID()..'1337'
    local data = spickle({
        gender = h.gender
        ,active = h.active
    })

    SaveSystemNotes(data, id)
end

-- ============
-- Load config.
-- ============
-- Loads the configuration from the system notes.
function humaniform.load_config()
    local h = humaniform

    -- We need to keep trying to load the config while GetCharacterID is
    -- returning nil (why should it, at all?).
    if not GetCharacterID() then
        local config_timer = Timer()
        config_timer:SetTimeout(500, h.load_config)
    else
        local id = GetCharacterID()..'1337'
        local data = unspickle(LoadSystemNotes(id))

        h.gender = data.gender
        h.active = data.active
    end
end

-- ======
-- Start.
-- ======
-- Loads the config and starts acting human.
function humaniform.start()
    local h = humaniform
    h.load_config()

    if h.active then
        h.schedule()
    end
end


-- =============
-- User command.
-- =============
-- Takes user input to change the configured gender or turn the behavior on or
-- off 
function humaniform.cmd(data, args)
    local h = humaniform

    if args and #args > 0 then
        local command = args[1]

        if command == 'male' or command == 'female' or command == 'neuter' then
            h.gender = command
            h.save_config()
            print('You\'re now acting like a '..command..'.')

        elseif command == 'off' then
            if h.timer:IsActive() then
                h.timer:Kill()
                h.active = false
                print('Humaniform has been turned off.')
            else
                print('Humaniform is already off.')
            end

        elseif command == 'on' then
            if not h.timer:IsActive() then
                h.schedule()
                print('Humaniform has been turned on.')
            else
                print('Humaniform is already on.')
            end
        end
    end
end

RegisterUserCommand('humaniform', humaniform.cmd)
RegisterEvent(humaniform.start, 'PLAYER_ENTERED_GAME')
