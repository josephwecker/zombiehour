-module(character).
-export([observe_characters/1, find_closest/2]).

observe_characters(Character) ->
  ScenarioMap = dict:fetch(map, Character),
  VisibleTiles = dict:fetch(visible_tiles, Character),
  {X,Y} = nav:position(dict:fetch(location, Character)),
  lists:flatten(
  lists:map(
    fun(Row) ->
        lists:flatten(
        lists:map(
          fun(Col) ->
              Key = tile:coords_to_key( Col, Row ),
              case lists:member(Key, VisibleTiles) of
                false ->
                  [];
                true ->
                  {Key, Tile} = digraph:vertex(ScenarioMap, Key),
                  case dict:fetch(character, Tile) of
                    nil ->
                      [];
                    OtherCharacter ->
                      case dict:fetch(zombified, OtherCharacter) of
                        false ->
                          {Col, Row};
                        true ->
                          []
                      end
                  end
              end
          end,
          lists:seq(X-7,X+7)))
    end,
    lists:seq(Y-7,Y+7))).

find_closest(Origin, Characters) ->
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

