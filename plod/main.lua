-- =====
-- Plod.
-- =====
-- Author: INKling
--
-- Plod can be used to plot courses between two sectors. It will also buy and
-- unload cargo between two stations, if you provide a third argument. For
-- trading it will do everything but the flying.
--
-- The trade item is case sensitive.
--
-- Usage.
-- ------
-- /plod [start sector or 'current'] [end sector or 'current'] [optional trade item]
--
-- Examples:
-- ---------
-- /plod 'sol e15' 'geira i5' 'XiRite Alloy'
-- /plod current 'geira o4' 'Basic Targeting Systems'
-- /plod a12 j6
plod = {}

plod.on = false
plod.to = nil
plod.from = nil
plod.trade_item = nil

plod.timing = {}
plod.timing.timer = Timer()
plod.timing.timer_interval = 1000
plod.timing.on = false

function plod.pinfo(str)
    print("\127c4a000plod: "..str.."\127o")
end

function plod.perr(str)
    print("\127a40000plod: "..str.."\127o")
end

function plod.ppass(str)
    print("\1274e9a06plod: "..str.."\127o")
end

-- ============
-- Plod: start.
-- ============
-- Starts plodding between two sectors.
function plod.cmd_plod(data, args)
    -- Unregister events to be sure that multiple calls don't increase the number of
    -- listeners.
    UnregisterEvent(plod.plodding_sector_changed, "SECTOR_CHANGED")
    UnregisterEvent(plod.plodding_trade, "ENTERED_STATION")

    -- No input.
    if not args then
        plod.pinfo("NAME")
        plod.pinfo("        plod - For plodding between two sectors and optionally moving cargo.")
        plod.pinfo("SYNOPSIS")
        plod.pinfo("        /plod [start sector] [end sector] [optional trade item]")
        plod.pinfo("EXAMPLES")
        plod.pinfo("        /plod 'sol e15' 'geira i5' 'XiRite Alloy'")
        plod.pinfo("        /plod current 'geira o4' 'Basic Targeting Systems'")
        plod.pinfo("        /plod a12 j6")
        return
    end

    -- Turn it off.
    if args[1] and args[1] == 'off' then
        plod.on = false
        plod.ppass("We're no longer tracking you.")
        return
    end

    if not args[1] or not args[2] then
        plod.perr("You must provide two sector names.")
        return
    end

    -- Plot course.
    local from = nil
    local current_system_name = string.gsub(SystemNames[GetCurrentSystemid()], '(%S+)%s*(%S*)', '%1')
    if (args[1] == 'current' or args[1] == '.') then
        from = GetCurrentSectorid()
    elseif string.find(args[1], '^%a%--%d+') then
        from = SectorIDFromLocationStr(current_system_name..' '..args[1])
    else
        from = SectorIDFromLocationStr(args[1])
    end

    local to = nil
    if (args[2] == 'current') then
        to = GetCurrentSectorid()
    elseif string.find(args[2], '^%a%--%d+') then
        to = SectorIDFromLocationStr(current_system_name..' '..args[2])
    else
        to = SectorIDFromLocationStr(args[2])
    end

    plod.pinfo("Plodding you between " .. LocationStr(from) .. " and " .. LocationStr(to) .. ".")
    NavRoute.SetFinalDestination(to)

    plod.from = from
    plod.to = to
    plod.on = true

    RegisterEvent(plod.plodding_sector_changed, "SECTOR_CHANGED")

    -- If they've provided a third argument, this is the item they're trading.
    if args[3] then
        local trade_item = args[3]
        plod.trade_item = trade_item
        plod.pinfo("We'll also move "..plod.trade_item.." for you.")

        RegisterEvent(plod.plodding_trade, "ENTERED_STATION")
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

        plod.perr("We weren't able to unload "..item..". This station doesn't sell it either. You probably made a mistake.")
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

RegisterUserCommand('plod', plod.cmd_plod)
