{-# LANGUAGE PatternGuards #-}

module State where
import World
import Graphics.Gloss.Game

data State
	= State
	{ stateWorld		:: World
	, stateLineStart	:: Point
	, stateLineEnd		:: Point }


initState world
	= State
	{ stateWorld		= world
	, stateLineStart	= (10, 10)
	, stateLineEnd		= (10, 10) }
	

handleInput :: World -> Event -> State -> State
handleInput world (EventKey key keyState mods pos) state
	| MouseButton LeftButton	<- key
	, Down				<- keyState
	, shift mods == Down	
	= state { stateLineEnd = worldPosOfWindowPos world pos }

	| MouseButton LeftButton	<- key
	, Down				<- keyState
	= state { stateLineStart 	= worldPosOfWindowPos world pos 
		, stateLineEnd		= worldPosOfWindowPos world pos }

	| MouseButton RightButton	<- key
	, Down				<- keyState
	= state { stateLineEnd 		= worldPosOfWindowPos world pos }

handleInput _ _ state
	= state
