function _update()
	-- SPLIT FRAME (prototype, plans/room60-overnight-2026-09-28.md): one
	-- frame is two steps. Part a (`__phase` nil): the timers, freeze,
	-- restart, and every object up to and including the PLAYER'S MOVE;
	-- part b: the player's update (the buttons), the objects after it, the
	-- tail, and `_draw`. `__phase` marks a state between the parts, `__frozen`
	-- a frozen frame's, whose part b does nothing. Both hold a TABLE: a
	-- global is a fixed slot of the block model, so a number there would be
	-- a value, not a shape, and the parts would share a kernel.
	if __phase == nil then
		frames=((frames+1)%30)
		if frames==0 and level_index()<30 then
			seconds=((seconds+1)%60)
			if seconds==0 then
				minutes = minutes + (1)
			end
		end

		if music_timer>0 then
		 music_timer = music_timer - (1)
		 if music_timer<=0 then
		  music(10,0,7)
		 end
		end

		if sfx_timer>0 then
		 sfx_timer = sfx_timer - (1)
		end

		-- cancel if freeze
		if freeze>0 then freeze = freeze - 1 __phase = {} __frozen = {} return end

		-- screenshake
		if shake>0 then
			shake = shake - (1)
			camera()
			if shake>0 then
				camera(-2+rnd(5),-2+rnd(5))
			end
		end

		-- restart (soon)
		if will_restart and delay_restart>0 then
			delay_restart = delay_restart - (1)
			if delay_restart<=0 then
				will_restart=false
				load_room(room.x,room.y)
			end
		end

		-- update each object, up to the player's move
		foreach(objects,function(obj)
			if __phase ~= nil then return end
			if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
				obj.move(obj.spd.x,obj.spd.y)
			end
			if obj.type == player then __phase = {} return end
			if obj.type.update~=nil then
				obj.type.update(obj)
			end
		end)
		if __phase == nil then __phase = {} end
		return
	end

	__phase = nil
	if __frozen then __frozen = nil return end

	local past = false
	foreach(objects,function(obj)
		if not past then
			if obj.type == player then
				past = true
				if obj.type.update~=nil then
					obj.type.update(obj)
				end
			end
			return
		end
		if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
			obj.move(obj.spd.x,obj.spd.y)
		end
		if obj.type.update~=nil then
			obj.type.update(obj)
		end
	end)

	-- start game
	if is_title() then
		if not start_game and (btn(k_jump) or btn(k_dash)) then
			start_game_flash=50
			start_game=true
		end
		if start_game then
			start_game_flash = start_game_flash - (1)
			if start_game_flash<=-30 then
				begin_game()
			end
		end
	end
end
