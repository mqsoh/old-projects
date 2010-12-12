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
humaniform.gender_names = { 'neuter', 'female', 'male' }
humaniform.actions = nil
humaniform.timer = Timer()

humaniform.default_actions = {
    'farts.'
    ,'burps.'
    ,'scratches {its} head.'
    ,'picks {its} nose.'
    ,'sighs.'
    ,'claps like a maniac!'
    ,'scratches {its} butt.'
    ,'strokes {its} chin in thought.'
    ,'rubs {its} eyes.'
    ,'twitches.'
    ,'takes a sip of {its} drink.'
}

humaniform.time_min_s = 5 * 60
humaniform.time_max_s = 20 * 60
humaniform.gender = humaniform.NEUTER
humaniform.active = true
humaniform.pending_actions = 0
humaniform.ui = {}
humaniform.ui.current_dialog = nil
humaniform.ui.closer = iup.stationbutton{
    title = 'closer'
    , action = function ()
        HideDialog(humaniform.ui.current_dialog)
    end
}

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
        local prepped = string.gsub(action, '{its}', h.possessive())

        SendChat('/me '..prepped, 'SECTOR')
    end

    h.schedule()
end

-- ===========
-- Possessive.
-- ===========
-- Returns a possessive pronoun for the configured gender.
function humaniform.possessive()
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

    if not data.actions then
        h.actions = {}
        for k, v in pairs(h.default_actions) do
            h.actions[k] = v
        end
    else
        h.actions = data.actions
    end

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
-- Instantiates a factory and all the GUI behavior.
function humaniform.ui.config()
    local h = humaniform

    local dialog, root_layout = unpack(h.ui.mkdialog('Humaniform -- act like a human!'))

    local toggle = h.ui.mktoggle('on / off', function (on_or_off)
        if on_or_off == 'on' then
            h.active = true
        else
            h.active = false
        end
        h.save_config()
    end)

    local gender = h.ui.mkgenderbender(h.gender, function (new_gender)
        h.gender = new_gender
        h.save_config()
    end)

    local action_box = iup.vbox{{}}
    local regenerate_actions = nil
    regenerate_actions = function ()
        local actions = h.ui.mkactions(
            h.actions
            , function (index, new_value)
                -- Edit.
                h.actions[index] = new_value
                h.save_config()
            end
            , function (index)
                -- Delete.
                table.remove(h.actions, index)
                h.save_config()
                regenerate_actions()
            end
        )
        local current = iup.GetNextChild(action_box)
        if current then
            current:destroy()
        end
        iup.Append(action_box, actions)
        iup.Refresh(action_box)
    end
    regenerate_actions()

    local buttons = h.ui.mkbuttons(
        'Add Action'
        , function ()
            h.actions[#h.actions + 1] = ''
            h.save_config()
            regenerate_actions()
        end
        , 'Reset Defaults'
        , function ()
            h.actions = {}
            for k, v in pairs(h.default_actions) do
                h.actions[k] = v
            end
            h.save_config()
            regenerate_actions()
        end
    )

    iup.Append(root_layout, toggle)
    iup.Append(root_layout, gender)
    iup.Append(root_layout, action_box)
    iup.Append(root_layout, buttons)

    h.ui.current_dialog = dialog
    ShowDialog(dialog)
end

-- ============
-- UI mkdialog.
-- ============
-- title
--     The title of the dialog window.
--
-- return
--     {dialog, root_layout} -- You need the dialog to show shit and the root
--     layout to add shit.
function humaniform.ui.mkdialog(title)
    local h = humaniform
    local root_layout = iup.vbox{}
    local dialog = iup.dialog{
        iup.vbox{
            iup.pdarootframe{
                iup.pdarootframebg{
                    root_layout
                }
            }
            , margin = '5x5'
        }
        , title = title
        , defaultesc = h.ui.closer
    }
    return {dialog, root_layout}
end

-- ============
-- UI mktoggle.
-- ============
-- label
--     The label for the toggle.
--
-- callback
--     This will be called with 'on' or 'off'.
--
-- return
--     A layout with the toggle inside.
function humaniform.ui.mktoggle(label, callback)
    local h = humaniform
    local toggle = nil
    toggle = iup.stationtoggle{
        title = h.active and 'On    ' or 'Off    '
        , margin = '10x5'
        , value = h.active and 'on' or 'off'
        , action = function (state)
            if state.value == 'ON' then
                toggle.title = 'On'
                callback('on')
            else
                toggle.title = 'Off'
                callback('off')
            end
        end
    }

    local layout = iup.hbox{
        toggle
        , margin = '10x5'
    }

    return layout
end

-- ==================
-- UI mkgenderbender.
-- ==================
-- current_gender
--     A number representing a gender in the list.
--
-- callback
--     A callback to be executed when the list changes which will be passed the
--     index of the newly selected item. The state is the index in the list
--     which can be used as the gender config directly because they're the same
--     order as the constants defined above.
--
-- return
--     A layout that contains the combo box.
function humaniform.ui.mkgenderbender(current_gender, callback)
    local h = humaniform
    local list = iup.pdarootlist{
        'Neuter'
        , 'Female'
        , 'Male'
        , value = current_gender
        , action = function (text, item, state)
            callback(state)
        end
        , DROPDOWN = 'yes'
    }
    local layout = iup.hbox{
        iup.label{ title = 'Gender:' }
        , list
        , margin = '10x5'
        , gap = '10'
    }
    return layout
end

-- =============
-- UI mkactions.
-- =============
-- action_list
--     A table of items with action text.
--
-- edit_callback
--     When a user edits the fields, this will be executed and passed the
--     action's index in the list and the new value.
--
-- delete_callback
--     When a field is deleted, this callback will be executed, being passed
--     the index of the deleted item.
--
-- return
--     A layout with the action fields.
function humaniform.ui.mkactions(action_list, edit_callback, delete_callback)
    local h = humaniform
    local layout = iup.vbox{
        iup.hbox{
            iup.label{ title = 'Your avatar...' }
        }
        , margin = '10x5'
    }

    for k, v in pairs(action_list) do
        local action_builder = function ()
            local index = k
            local layout = nil

            layout = iup.hbox{
                iup.text{
                    value = v
                    , action = function (ud, char)
                        edit_callback(index, ud.value)
                    end
                    , size = '500x'
                }
                , iup.stationbutton{
                    title = 'X'
                    , action = function ()
                        delete_callback(index)
                    end
                }
            }

            return layout
        end
        iup.Append(layout, action_builder())
    end

    return layout
end

-- =============
-- UI mkbuttons.
-- =============
-- Makes the miscellaneous buttons on the bottom.
--
-- add_callback
--     Executed when you should add a field to the list, first creating
--     an entry in the table of actions and then regenerating the list.
--
-- reset_callback
--     Executed when the action list should be regenerated with the
--     default list.
--
-- return
--     A layout with the buttons.
function humaniform.ui.mkbuttons(add_label, add_callback, reset_label, reset_callback)
    local h = humaniform
    local layout = iup.hbox{
        iup.stationbutton{
            title = add_label
            , action = function ()
                add_callback()
            end
        }
        , iup.fill{}
        , iup.stationbutton{
            title = reset_label
            , action = function ()
                reset_callback()
            end
        }
        , margin = '10x5'
    }

    return layout
end

-- =============
-- User command.
-- =============
-- Takes user input to change the configured gender or turn the behavior on or
-- off 
function humaniform.cmd(data, args)
    local h = humaniform

    if not args then

        h.ui.config()

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
RegisterEvent(humaniform.start, 'PLAYER_ENTERED_GAME')
