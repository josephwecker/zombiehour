-module(character).
-export([find_target/1, find_target/2]).

find_target(Character) ->
  Tiles = dict:fetch(visible_tiles, Character),
  target(Character, Tiles).

find_target(Character, Direction) ->
  Origin = dict:fetch(location, Character),
  Tiles = nav:get_quadrant(Origin, Direction, dict:fetch(sight,Character)),
  VisibleTiles = dict:fetch(visible_tiles, Character),
  TargetTiles = [ X || X <- Tiles, lists:member(X, VisibleTiles) ],
  target(Character, TargetTiles).

target(Character, Tiles) ->
  Map = dict:fetch(map, Character),
  Targets = lists:flatmap(
    fun(Tile) ->
        {Tile, TileData} = digraph:vertex(Map, Tile),
        case dict:fetch(character, TileData) of
          nil ->
            [];
          OtherCharacter ->
            case dict:fetch(zombified, OtherCharacter) /= dict:fetch(zombified, Character) of
              true ->
                [Tile];
              false ->
                []
            end
        end
    end,
    Tiles),
  Origin = dict:fetch(location, Character),
  case Targets of
    [] ->
      false;
    [Target] ->
      Target;
    Targets ->
      find_closest(Origin, Targets)
  end.

find_closest(Origin, Characters) ->
  %io:format("~p~p~n",[Origin, Characters]),
  [H|T] = Characters,
  find_closest(Origin, T, H).

find_closest(_Origin, [], BestPick) ->
  BestPick;

find_closest(Origin, Characters, BestPick) ->
  [H|T] = Characters,
  BS = nav:distance(Origin, BestPick),
  HS = nav:distance(Origin, H),
  case HS < BS of
    true ->
      Winner = H;
    false ->
      Winner = BestPick
  end,
  find_closest(Origin, T, Winner).

