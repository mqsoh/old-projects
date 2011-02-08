-- =====
-- Plod.
-- =====
-- Author: INKling
--
-- Plod can be used to plot courses between two sectors. It will also buy and
-- unload cargo between two stations, if you provide it after the second sector
-- definition. For trading it will do everything but the flying.
--
-- The trade item is case sensitive.
--
-- Plod also has a mission mode. When active, it will listen for mission
-- updates and try to find an destination sector and an item to trade. This has
-- been tested mainly against hive skirmish and trading guild missions, though
-- it should work with any mission I haven't thought of.
--
-- Usage.
-- ------
-- /plod [from sector] [to sector] [optional trade item]
--
-- /plod mission
--     Turns on mission mode.
--
-- /plod off
--     Turns plod off.
--
-- Examples:
-- ---------
-- /plod sol e15 geira i5 XiRite Alloy
-- /plod . geira o4 Basic Targeting Systems
-- /plod a12 j6
plod = plod or {}

plod.on = false
plod.to = nil
plod.from = nil
plod.trade_item = nil

plod.timing = {}
plod.timing.timer = Timer()
plod.timing.timer_interval = 1000
plod.timing.on = false

plod.mission_watch = false
plod.mission_last_message = nil

function plod.pinfo(str)
    print("\127c4a000plod: "..tostring(str).."\127o")
end

function plod.perr(str)
    print("\127a40000plod: "..tostring(str).."\127o")
end

function plod.ppass(str)
    print("\1274e9a06plod: "..tostring(str).."\127o")
end

-- ============
-- Plod: usage.
-- ============
function plod.usage()
        plod.pinfo("NAME")
        plod.pinfo("        plod - For plodding between two sectors and optionally moving cargo.")
        plod.pinfo("SYNOPSIS")
        plod.pinfo("        /plod [start sector] [end sector] [optional trade item]")
        plod.pinfo("EXAMPLES")
        plod.pinfo("        /plod sol e15 geira i5 XiRite Alloy")
        plod.pinfo("        /plod . geira o4 Basic Targeting Systems")
        plod.pinfo("        /plod a12 j6")
end

-- ============
-- Plod: reset.
-- ============
-- Resets the application state.
function plod.reset()
    plod.on = false
    plod.to = nil
    plod.from = nil
    plod.trade_item = nil
    plod.mission_watch = false

    -- Unregister events to be sure that multiple calls don't increase the number of
    -- listeners.
    UnregisterEvent(plod.plodding_sector_changed, "SECTOR_CHANGED")
    UnregisterEvent(plod.plodding_trade, "ENTERED_STATION")
end

-- ====================
-- Plod: get sector ID.
-- ====================
-- args
--     A list of arguments to parse. This function is destructive and removes
--     leading elements as it parses.
-- return
--     A sector ID or nil.
function plod.get_sector_id(args)
    if not args then
        return nil
    end

    local sector_id = nil
    local system_name = nil

    while sector_id == nil and # args > 0 do
        local arg = args[1]

        -- Current sector.
        if arg == '.' or arg == 'current' then
            sector_id = GetCurrentSectorid()

        -- Sector definition.
        elseif arg:match("%a%-?%d") then
            if not system_name then
                system_name = string.gsub(SystemNames[GetCurrentSystemid()], '(%S+)%s*(%S*)', '%1')
            end
            sector_id = SectorIDFromLocationStr(system_name..' '..arg)

        -- System definition.
        elseif arg:match("%w+") then
            if not system_name then
                system_name = arg
            else
                -- We already found a system name and got another. System names
                -- are one word.
                return nil
            end

        -- Final error condition.
        else
            return nil

        end

        table.remove(args, 1)
    end

    return sector_id
end

-- =================
-- Plod: user input.
-- =================
-- Starts plodding between two sectors.
function plod.cmd_plod(data, args)
    plod.reset()

    -- No input.
    if not args then
        plod.usage()
    end

    -- Turn it off.
    if args[1] and args[1] == 'off' then
        -- We should have already reset, so just tell the user.
        plod.ppass("We're no longer tracking you.")
        return
    end

    -- Listen for a mission.
    if args[1] and args[1] == 'mission' then
        plod.on = true
        plod.mission_watch = true
        plod.pinfo('Trying last mission update.')
        plod.mission_update(plod.mission_last_message)
        return
    end

    -- Manually defined course.
    plod.on = true
    local from = plod.get_sector_id(args)
    local to = plod.get_sector_id(args)
    local trade_item = nil

    if not from or not to then
        plod.usage()
        plod.reset()
        plod.perr('We weren\'t able to determine the start and end sectors.')
        return
    end

    -- If there are any arguments left, it's the trade item.
    if # args > 0 then
        trade_item = table.concat(args, ' ')
    end

    plod.start(from, to, trade_item)
end

-- ============
-- Plod: start.
-- ============
-- This will set the application state and register the events.
--
-- from
--     The sectorid of the start sector.
-- to
--     The sectorid of the end sector.
-- item
--     Optional. The item to trade.
function plod.start(from, to, item)
    plod.from = from
    plod.to = to
    plod.trade_item = item

    plod.pinfo("Plodding you between " .. LocationStr(from) .. " and " .. LocationStr(to) .. ".")

    if item then
        plod.pinfo("We'll also move "..item.." for you.")

        RegisterEvent(plod.plodding_trade, "ENTERED_STATION")
    end
    RegisterEvent(plod.plodding_sector_changed, "SECTOR_CHANGED")

    NavRoute.SetFinalDestination(to)
end

-- ====================
-- Plod: sector update.
-- ====================
-- If plod's on, this function will be registered to the SECTOR_CHANGED event.
function plod.plodding_sector_changed(event, sectorid)
    if sectorid == plod.to or sectorid == plod.from then
        local new_destination = nil
        if sectorid == plod.to then
            new_destination = plod.from
        else
            new_destination = plod.to
        end

        plod.pinfo("You've arrived. We're plodding a new course to "..LocationStr(new_destination)..".")
        NavRoute.SetFinalDestination(new_destination)

        if plod.trade_item then
            plod.pinfo("Dock to make sure you trade "..plod.trade_item..".")
        end
    end
end

-- ============
-- Plod: trade.
-- ============
-- If a trade item was defined, then this will be registered with
-- ENTERED_STATION event.
function plod.plodding_trade(event)
    if not plod.trade_item then
        return
    end

    -- Convert the trade item into an item ID.
    local item = plod.trade_item
    local itemid = 0
    for id = 1, GetNumStationMerch() do
        local info = GetStationMerchInfo(id)
        if info.name == item then
            itemid = info.itemid
        end
    end

    if itemid == 0 then
        -- Unload item.
        -- ------------
        -- If station doesn't have this item, we'll try unloading it if it's in the
        -- cargo hold.
        local shipid = GetActiveShipID()
        local inventory = GetShipInventory(shipid)

        if inventory['cargo'] then
            for _, id in pairs(inventory['cargo']) do
                local name = GetInventoryItemName(id)
                local num = GetInventoryItemQuantity(id)

                if name == item then
                    CheckStorageAndUnloadCargo({{itemid = id, quantity = num}})
                    plod.ppass("We unloaded your "..name..".")
                    return
                end
            end
        end

        plod.perr("We weren't able to unload "..item..". This station doesn't sell it either.")
    else
        -- Buy item.
        -- ---------
        -- If the station has the trade item, we should buy it.
        local max_cargo = GetActiveShipMaxCargo()
        PurchaseMerchandiseItem(itemid, max_cargo, function (status)
            if not status then
                plod.ppass("We purchased your cargo.")
            else
                plod.perr("We failed to purchase your cargo.")
            end
        end)
    end

    if itemid then
    end
end

-- ====================
-- Plod: mission event.
-- ====================
-- Callback for the CHAT_MSG_SECTORD_MISSION and CHAT_MSG_MISSION events to
-- store the last mission message and, if enabled, execute the plod mission
-- behavior.
--
-- event
--     The name of the event.
--
-- data
--     A table: { missionid = ..., msg = ... }
function plod.mission_event(event, data)
    if not data then
        return
    end

    local msg = data.msg

    -- If plod is turned on after accepting a mission we want the last
    -- message, so we'll always store the last message.
    plod.mission_last_message = msg

    if plod.on and plod.mission_watch then
        plod.mission_update(msg)
    end
end

-- =====================
-- Plod: mission update.
-- =====================
-- message
--     The mission text to parse.
function plod.mission_update(message)
    if not message then
        plod.pinfo('We\'re waiting for a mission update.')
        return
    end

    local items = {}
    local sectors = {}
    local cargo_pat = '(%d+) (%w+ ?%w* ?%w*) crates'
    local sector_pat = '(%u%w+) ?(%u?%w*) (%u%-%d+)'

    for number, item in message:gmatch(cargo_pat) do
        table.insert(items, item)
    end
    for system_name1, system_name2, sector in message:gmatch(sector_pat) do
        table.insert(sectors, system_name1..' '..sector)
    end

    -- We only want to start plod if there's one sector. If there are more,
    -- they probably haven't selected a mission yet and we got updates from
    -- mission menus.
    if # sectors == 1 then
        -- Always start from the current sector for missions.
        local from = GetCurrentSectorid()
        local to = SectorIDFromLocationStr(sectors[1])
        local item = nil
        if items and # items == 1 then
            item = items[1]
        end

        if to == nil or from == nil then
            plod.perr('There was an error looking up the sector ID.')
        end

        -- If we've calculated a sector we're already plodding between, then
        -- we'll ignore the update. Otherwise docking at a station during a
        -- mission will cause us to restart the plod.
        if to ~= plod.to and to ~= plod.from then
            plod.start(from, to, item)
        end
    end
end

RegisterUserCommand('plod', plod.cmd_plod)
RegisterEvent(plod.mission_event, 'CHAT_MSG_SECTORD_MISSION')
RegisterEvent(plod.mission_event, 'CHAT_MSG_MISSION')
