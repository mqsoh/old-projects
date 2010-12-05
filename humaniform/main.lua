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
-- /humaniform -- opens a config dialog.
-- /humaniform [gender, 'on' or 'off']
--
-- Examples.
-- ---------
-- /humaniform
-- /humaniform on
-- /humaniform off
-- /humaniform male
-- /humaniform female
-- /humaniform neuter
humaniform = humaniform or {}
humaniform.NEUTER = 1
humaniform.FEMALE = 2
humaniform.MALE = 3
humaniform.actions = nil
humaniform.timer = Timer()

humaniform.default_actions = {
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
humaniform.gender = humaniform.NEUTER
humaniform.active = true
humaniform.pending_actions = 0

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

    if action then
        local prepped = string.gsub(action, '{posessive}', h.posessive())

        SendChat('/me '..prepped, 'SECTOR')
    end

    h.schedule()
end

-- ===========
-- Possessive.
-- ===========
-- Returns a posessive pronoun for the configured gender.
function humaniform.posessive()
    local h = humaniform
    if h.gender == h.MALE then
        return 'his'
    elseif h.gender == h.FEMALE then
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
        ,actions = h.actions
    })

    SaveSystemNotes(data, id)
end

-- ============
-- Load config.
-- ============
-- Loads the configuration from the system notes.
function humaniform.load_config()
    local h = humaniform

    if not GetCharacterID() then
        humaniform.timer:SetTimeout(5000, humaniform.load_config)
        return false
    end

    local id = GetCharacterID()..'1337'
    local data = unspickle(LoadSystemNotes(id))

    -- We have to check for the old string just in case someone has old
    -- settings.
    if data.gender == 'neuter' then
        h.gender = h.NEUTER
    elseif data.gender == 'female' then
        h.gender = h.FEMALE
    elseif data.gender == 'male' then
        h.gender = h.MALE
    else
        h.gender = data.gender
    end

    h.active = data.active or true
    h.actions = data.actions or humaniform.default_actions

    return true
end

-- ======
-- Start.
-- ======
-- Loads the config and starts acting human.
function humaniform.start()
    local h = humaniform

    -- We need to keep trying to load the config while GetCharacterID is
    -- returning nil (why should it, at all?).
    if not h.load_config() then
        h.timer:SetTimeout(500, humaniform.start)
        return
    end

    if h.active then
        h.schedule()
    end
end

-- ===========
-- GUI config.
-- ===========
function humaniform.gui_config()
    local h = humaniform

    -- We'll declare these variables so that they can be used in callbacks.
    local d = nil
    local active = nil
    local genders = nil
    local actions = nil
    local add = nil

    humaniform.pending_actions = 0

    -- Active toggle.
    active = iup.hbox{
        {};margin = '10x10', gap = '5'
    }
    iup.Append(active, iup.stationtoggle{
        margin = '10x10',
        value = h.active and 'on' or 'off',
        action = function (state)
            -- Enable / disable.
            if state.value == 'ON' then
                humaniform.active = true
                if not humaniform.timer:IsActive() then
                    humaniform.schedule()
                end
            else
                humaniform.active = false
                if humaniform.timer:IsActive() then
                    humaniform.timer:Kill()
                end
            end
            humaniform.save_config()
        end
    })
    iup.Append(active, iup.label{
        title = 'Active'
    })

    -- Gender selection.
    genders = iup.hbox{
        iup.label{ title = 'Select a gender.' },
        iup.list{
            'Neuter',
            'Female',
            'Male',
            action = function (text, item, state)
                -- Gender bender.
                -- 'state' is the position in the list which, thanks to ordering
                -- the same as the defined constants, we can just pass off to
                -- the gender config.
                humaniform.gender = state
                humaniform.save_config()
            end
            ;DROPDOWN = 'yes', VALUE = h.gender
        }
        ;margin = '10x10', gap = '10'
    }

    -- Action inputs.
    actions = iup.vbox{
        iup.label{
            title = 'Your avatar...'
        }
        ;margin = '10x10', gap = '1'
    }
    for index, act in pairs(h.actions) do
        local closure = function ()
            local box = nil
            local index = index
            box = iup.hbox{
                iup.text{
                    value = act,
                    action = function (ud, char)
                        -- Edit action.
                        humaniform.actions[index] = ud.value
                        humaniform.save_config()
                    end
                    ;size = '500x'
                },
                iup.button{
                    title = 'X',
                    action = function ()
                        -- Remove action.
                        print(index)
                        table.remove(humaniform.actions, index)
                        humaniform.save_config()
                        box:detach()
                        iup.Refresh(actions)
                    end
                }
            }
            iup.Append(
                actions,
                box
            )
        end
        closure()
    end

    -- Add button.
    add = iup.hbox{
        iup.button{
            title = 'Add Action',
            action = function ()
                humaniform.pending_actions = humaniform.pending_actions + 1
                local index = #humaniform.actions + 1 + humaniform.pending_actions
                iup.Append(
                    actions,
                    iup.text{
                        value = '',
                        action = function (ud, char)
                            -- Edit action.
                            humaniform.actions[index] = ud.value
                            humaniform.save_config()
                        end
                        ;size = '500x'
                    }
                )
                d:show()
            end
        },
        iup.fill{},
        iup.button{
            title = 'Reset Default Actions',
            action = function ()
                humaniform.actions = humaniform.default_actions
                humaniform.save_config()
                humaniform.load_config()

                d:hide()
                local timer = Timer()
                timer:SetTimeout(1, humaniform.gui_config)
            end
        }
        ;margin = '10x10'
    }

    -- Build the dialog.
    d = iup.dialog{
        iup.vbox{
            active,
            genders,
            actions,
            add
        }
        ;title = 'Humaniform -- act like a human!'
    }

    d:show()
end

-- =============
-- User command.
-- =============
-- Takes user input to change the configured gender or turn the behavior on or
-- off 
function humaniform.cmd(data, args)
    local h = humaniform

    if not args then

        h.gui_config()

    elseif args and #args > 0 then
        local command = args[1]

        if command == 'neuter' then
            h.gender = h.NEUTER
            h.save_config()
            print('You\'re now acting like a '..command..'.')

        elseif command == 'male' then
            h.gender = h.MALE
            h.save_config()
            print('You\'re now acting like a '..command..'.')

        elseif command == 'female' then
            h.gender = h.FEMALE
            h.save_config()
            print('You\'re now acting like a '..command..'.')

        elseif command == 'off' then
            if h.timer:IsActive() then
                h.timer:Kill()
                h.active = false
                h.save_config()
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
RegisterUserCommand('hf', humaniform.cmd)
RegisterEvent(humaniform.start, 'PLAYER_ENTERED_GAME')
