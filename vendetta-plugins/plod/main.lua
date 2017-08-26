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

plod.FORWARD = 1
plod.BACKWARD = 0

plod.on = false
plod.to = nil
plod.from = nil
plod.direction = plod.FORWARD
plod.trade_item = nil
plod.auto_trade = false
plod.auto_trade_item = nil

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
        plod.pinfo("        /plod off")
        plod.pinfo("            Turn plod off.")
        plod.pinfo("        /plod mission")
        plod.pinfo("            Start listening for mission updates.")
        plod.pinfo("        /plod [start sector] [end sector] [optional trade item]")
        plod.pinfo("            Manually defined behavior.")
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
    plod.auto_trade = false
    plod.auto_trade_item = nil
    plod.mission_watch = false
    plod.direction = 1

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
        --plod.auto_trade = true
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
        if args[# args] == 'auto' then
            plod.auto_trade = true
            table.remove(args, # args)
        end

        -- Check again because we might have just lopped off an 'auto'.
        if # args > 0 then
            trade_item = table.concat(args, ' ')
        end
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
    end

    if item or plod.auto_trade then
        RegisterEvent(plod.plodding_trade, "ENTERED_STATION")
    end

    RegisterEvent(plod.plodding_sector_changed, "SECTOR_CHANGED")

    NavRoute.SetFinalDestination(to)
    plod.direction = plod.FORWARD

    -- We'll fake an entered station event if the player is in a station.
    if PlayerInStation() then
        plod.plodding_trade('ENTERED_STATION')
    end
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
            plod.direction = plod.BACKWARD
        else
            new_destination = plod.to
            plod.direction = plod.FORWARD
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
    plod.perr('trade event')

    if not plod.trade_item and not plod.auto_trade then
        return
    end

    local shipid = GetActiveShipID()
    local inventory = GetShipInventory(shipid)
    local continued = false

    local maybe_sell = function (list, continue)
        for _, id in pairs(list) do
            local name = GetInventoryItemName(id)
            local num = GetInventoryItemQuantity(id)

            log_print('    '..name)
            if name == plod.trade_item then
                log_print('        trade item')
                -- Unload the manually defined item because they might want to
                -- stock up.
                CheckStorageAndUnloadCargo({{itemid = id, quantity = num}}, function (status)
                    if not status then
                        plod.ppass('We unloaded '..num..'cu of '..name..'.')
                    else
                        plod.perr('We failed to unload '..num..'cu of '..name..'.')
                    end
                    plod.timing.timer:SetTimeout(2000, continue)
                end)
                return true
            elseif name == plod.auto_trade_item then
                log_print('        auto item')
                -- Sell anything else (which should be an auto-determined item).
                --SellInventoryItem(id, num)
                UnloadSellCargo({{itemid=id, quantity=num}}, function (status)
                    if not status then
                        plod.ppass('We sold '..num..'cu of '..name..'.')
                    else
                        plod.perr('We failed to sell '..num..'cu of '..name..'.')
                    end
                    plod.timing.timer:SetTimeout(2000, continue)
                end)
                return true
            end
        end
        return false
    end

    -- This function will be executed when the asynchronous selling function
    -- completes.
    local continue_function = function ()
        if continued then
            return
        end

        continued = true

        if plod.trade_item and not plod.auto_trade then
            -- Only trading the defined item.
            local trade_status = plod.buy_item(plod.trade_item)
            if trade_status == 2 then
                plod.pinfo('This station doesn\'t sell '..plod.trade_item..'. This is okay if the other station does.')
            elseif trade_status ~= 0 then
                plod.perr('We failed to purchase '..plod.trade_item..'.')
            end
        elseif not plod.trade_item and plod.auto_trade then
            -- Only auto-trading.
            local auto_item = plod.find_auto_trade_item()
            if not auto_item then
                plod.pinfo('We were unable to find a suitable item to trade for your next trip.')
                return
            end

            local status = plod.buy_item(auto_item)
            if status ~= 0 then
                plod.perr('We failed to buy '..auto_item..' for you.')
            else
                plod.ppass('We bought '..auto_item..' for you.')
                plod.auto_trade_item = auto_item
            end
        else
            -- Mixed trading.
            local trade_status = plod.buy_item(plod.trade_item)
            if trade_status == 2 then
                local auto_item = plod.find_auto_trade_item()
                if not auto_item then
                    plod.pinfo('This station doesn\'t sell '..plod.trade_item..' and we were unable to determine a suitable item to auto trade.')
                    return
                end

                local auto_status = plod.buy_item(auto_item)
                if auto_status == 0 then
                    plod.auto_trade_item = auto_item
                    plod.ppass('We bought '..auto_item..' for you.')
                else
                    plod.perr('We failed to purchase a suitable item for trading.')
                end
            elseif trade_status ~= 0 then
                plod.perr('We failed to purchase '..plod.trade_item..'.')
            end
        end
    end

    log_print('Poot! trade item: '..tostring(plod.trade_item)..', auto: '..tostring(plod.auto_trade_item))
    log_print('Cargo selling:')
    local selling = false
    if inventory['cargo'] then
        selling = maybe_sell(inventory['cargo'], continue_function)
    end

    log_print('Addon selling:')
    if not selling and inventory['addons'] then
        selling = maybe_sell(inventory['addons'], continue_function)
    end

    if not selling then
        continue_function()
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

-- ===========================
-- Plod: find auto trade item.
-- ===========================
-- This will try and find a suitable trade item for the next trip.
--
-- return
--     The name of a commodity or nil.
function plod.find_auto_trade_item()
    local item = nil

    local stations, goods = unpack(plod.get_tadata())

    local from_sector = GetCurrentSectorid()
    local to_sector = nil
    if from_sector == plod.from then
        to_sector = plod.to
    else
        to_sector = plod.from
    end

    local from_station = nil
    local to_station = nil

    for stationid, info in pairs(stations) do
        if info.sectorid == from_sector then
            --from_station = stations[stationid]
            from_station = stationid
        elseif info.sectorid == to_sector then
            --to_station = stations[stationid]
            to_station = stationid
        end
    end

    if not from_station or not to_station then
        plod.pinfo('We don\'t have have Trade Assist data for both stations.')
    else
        -- We have TA data for both stations.
        local suitable_items = {}

        for good_name, good_info in pairs(goods) do
            if good_info and type(good_info) == 'table' and plod.valid_good(good_info.type) then

                local volume = good_info.vol
                local good_for_to, good_for_from

                for station_id, station_info in pairs(good_info) do
                    if station_id == from_station then
                        good_for_from = station_info
                    elseif station_id == to_station then
                        good_for_to = station_info
                    end
                end

                if good_for_to and good_for_from then
                    local buy_data = nil
                    local sell_data = nil

                    if plod.FORWARD then
                        buy_data = good_for_to
                        sell_data = good_for_from
                    else
                        buy_data = good_for_from
                        sell_data = good_for_to
                    end

                    if buy_data.buy and sell_data.sell and (buy_data.buy - sell_data.sell > 0)then
                        -- Adding suitable item.
                        table.insert(suitable_items, {
                            name = good_name,
                            profit = buy_data.buy - sell_data.sell,
                            volume = volume
                        })
                    end
                end

            end
        end

        if # suitable_items > 0 then
            -- Find the best suitable item.
            local highest_profit_per_cu = 0
            for _, suitable_item in ipairs(suitable_items) do
                local profit_per_cu = suitable_item.profit / suitable_item.volume
                if profit_per_cu > highest_profit_per_cu then
                    item = suitable_item.name
                end
            end
        end
    end

    return item
end

-- ============================
-- Plod: Get Trade Assist data.
-- ============================
-- return
--     A list of {table stations, table goods}
function plod.get_tadata()
    -- We'll load this every time because TA data might change as we're
    -- plodding.
    local taid = GetCharacterID()..'201'
    local tadata = unspickle(LoadSystemNotes(taid)) or {}
    local goods = tadata.goods or {}
    local stations = tadata.stations or {}

    return {stations, goods}
end

-- ===============
-- Plod: buy item.
-- ===============
-- name
--     The case-insensitive name of a commodity.
--
-- return
--     0 -- success
--     1 -- No room in the hold.
--     2 -- Not sold here.
--     3 -- API failure.
--     4 -- Other.
function plod.buy_item(name)
    name = string.lower(name)
    local max_space = GetActiveShipMaxCargo()
    local used_space = GetActiveShipCargoCount()
    local available_space = max_space - used_space

    if available_space < 1 then
        return 1
    end

    -- Convert the name into an item ID.
    local itemid = nil
    local item_count = 0
    -- If this isn't a commodity, it will have to be manually loaded into the
    -- hold.
    local load_on_purchase = false
    for id = 1, GetNumStationMerch() do
        local info = GetStationMerchInfo(id)
        local volume = info.volume
        if string.lower(info.name) == name then
            itemid = info.itemid
            item_count = math.floor(available_space / volume)
            load_on_purchase = not (info.type == 'commodities')

            log_print(info.name..' info:')
            for k, v in pairs(info) do
                log_print('    '..tostring(k)..': '..tostring(v))
            end
        end
    end

    if itemid == nil then
        return 2
    end

    PurchaseMerchandiseItem(itemid, item_count, function (status)
        if not status then
            plod.ppass('We purchased '..tostring(available_space)..'cu ('..tostring(item_count)..' units)'..' of '..name..'.')

            if load_on_purchase then
                local cargo = GetStationCargoList()
                if cargo and type(cargo) == 'table' then
                    for _, cargo_id in ipairs(cargo) do
                        local n = string.lower(GetInventoryItemName(cargo_id))
                        if n == name then
                            LoadCargo({{itemid=cargo_id, quantity=item_count}})
                        end
                    end
                end
            end
        else
            plod.perr('We failed to purchase '..name..'.')
        end
    end)

    return 0
end

function plod.valid_good(good_type)
    local valid_types = {
        'lightweapon',
        'turretweapon',
        'commodities',
        'battery',
        'heavyweapon'
    }

    for _, t in ipairs(valid_types) do
        if t == good_type then
            return true
        end
    end

    return false
end

RegisterUserCommand('plod', plod.cmd_plod)
RegisterEvent(plod.mission_event, 'CHAT_MSG_SECTORD_MISSION')
RegisterEvent(plod.mission_event, 'CHAT_MSG_MISSION')
