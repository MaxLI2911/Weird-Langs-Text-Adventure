/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1, hp/1.
:- dynamic goblin_state/1, chest_state/1, fire_elemental_state/1.
:- dynamic in/2, held_by/2.
:- dynamic dwarf_state/1, door_state/1.
:- dynamic door_state_final/1.
:- dynamic path/3.

:- retractall(at(_, _)),
   retractall(i_am_at(_)),
   retractall(hp(_)),
   retractall(goblin_state(_)),
   retractall(chest_state(_)),
   retractall(fire_elemental_state(_)),
   retractall(in(_, _)),
   retractall(held_by(_, _)),
    retractall(dwarf_state(_)), 
    retractall(door_state(_)),
    retractall(door_state_final(_)).

/* world gen */

% Rooms
i_am_at(room_a).

path(room_a, e, room_b).  
path(room_b, w, room_a).
path(room_b, e, room_c).  
path(room_c, w, room_b).
path(room_c, e, room_d).  
path(room_d, w, room_c).
path(room_b, n, storage_room).      
path(storage_room, s, room_b).     
path(storage_room, n, brewery_room).   
path(brewery_room, s, storage_room).
path(storage_room, e, dwarfs_room).      
path(dwarfs_room, w, storage_room).
path(dwarfs_room, s, room_c). 
path(room_c, n, dwarfs_room).
path(rune_room, w, dwarfs_room).
path(merchant_room, w, rune_room).





% Actors / features as items you can "see" in rooms
at(goblin, room_a).
at(chest, room_b).
at(well, room_c).
at(fire_elemental, room_d).
at(wood_chest, storage_room).
at(brewing_apparatus, brewery_room).
at(dwarf, dwarfs_room).
at(locked_door, dwarfs_room).
at(mushrooms, rune_room).
at(final_door, rune_room).
at(merchant, merchant_room).

% Chest contents
in(chest, sword).
in(chest, gold_coin).
chest_state(closed).
in(wood_chest, malt).
in(wood_chest, salt).
chest_state(closed2).


% Goblin state and his bucket
goblin_state(alive).
held_by(bucket, goblin).   % he wears a bucket on his head

% Fire elemental state
fire_elemental_state(alive).

% dwarf and door state
dwarf_state(alive).            
door_state(locked).            
held_by(battle_axe, dwarf).

door_state_final(locked).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        % taking from an NPC that holds it (e.g., bucket on goblin) is not allowed
        held_by(X, goblin),
        goblin_state(alive),
        write('The goblin won''t let you take that from his head.'),
        !, nl.

take(X) :-
        held_by(X, dwarf),
        dwarf_state(alive),
        write('The dwarf won''t let you take his battle axe!'), nl,
        write('"This is my precious axe! Get your own!"'), nl,
        !, nl.


take(X) :-
        (X = locked_door ; X = final_door),
        write('You can''t take a door!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, 
        look.

go(_) :-
        write('You can''t go that way.').


check_door_access(_, _, _). 

/* List all possible exits from the current location */

exits :-
    i_am_at(Here),
    findall(Direction, path(Here, Direction, _), Directions),
    (   Directions = []
    ->  write('There are no exits from here.'), nl
    ;   write('You can go: '), nl,
        forall(member(D, Directions),
               (write('  - '), write(D), nl))
    ).


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        exits.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

describe(room_a) :-
    write('You are in a first room.'), nl,
    (   goblin_state(alive)
    ->  write('An ugly goblin is here!')
    ;   goblin_state(dead)
    ->  write('The body of a slain goblin is on the floor.')
    ;   write('The room is empty now.')
    ).

describe(room_b) :-
    write('This room is dusty.'), nl,
    (   chest_state(closed)
    ->  write('There is a closed wooden chest.')
    ;   write('There is an open wooden chest.')
    ).

describe(room_c) :-
    write('A room with a well in the middle.'), nl,
    write('The water looks surprisingly clean.').

describe(room_d) :-
    write('It''s incredibly hot in here!'), nl,
    (   fire_elemental_state(alive)
    ->  write('A swirling fire elemental crackles and pops in the corner.')
    ;   write('A pile of cool ash sits in the corner.')
    ).

describe(storage_room) :-
    write('A dusty storage room filled with old barrels and crates.'), nl,
    (   chest_state(closed2)
    ->  write('There is a small wooden chest in the corner.')
    ;   write('The small chest is open.')
    ).

describe(brewery_room) :-
    write('This room smells of hops and fermentation.'), nl,
    write('A complex brewing apparatus stands in the center, connected to copper pipes.'), nl,
    (   holding(beer)
    ->  write('You feel proud of your brewing skills!')
    ;   write('The apparatus looks ready for use.')
    ).

describe(dwarfs_room) :-
    write('A narrow stone corridor blocked by a massive iron door.'), nl,
    (   dwarf_state(alive)
    ->  write('A stout dwarf stands guard, blocking your path.'), nl,
        write('"None shall pass! My orders are clear!" he grumbles.'), nl
    ;   dwarf_state(distracted)
    ->  write('The dwarf is happily drinking your beer and singing dwarven songs.'), nl,
        write('He seems to have forgotten about his duty.'), nl
    ;   dwarf_state(dead)
    ->  write('The body of the dwarf lies on the stone floor.'), nl,
        (   door_state(unlocked)
        ->  write('The iron door is now unlocked.')
        ;   write('The iron door remains locked.')
        )
    ;   write('The corridor is empty now.'), nl
    ).

describe(rune_room) :-
    write('A circular chamber covered in ancient runes.'), nl,
    write('The walls glow with a soft blue light.'), nl,
    write('Inscription above the eastern door reads:'), nl,
    write('"Beer is a blessing and a curse"'), nl,
    write('Below it, letters are numbered and you can press them:'), nl,
    write('B-1, e-2, e-3, r-4, i-5, s-6, a-7, b-8, l-9, e-10, s-11, s-12, i-13, n-14, g-15, a-16, n-17, d-18, z-19, c-20, u-21, r-22, s-23, e-24'), nl,
    (   at(mushrooms, rune_room)
    ->  write('Strange glowing mushrooms grow in the corners.'), nl
    ;   true
    ).

describe(merchant_room) :-
    write('A small chamber illuminated by eerie purple flames.'), nl,
    write('The walls seem to absorb the light, creating deep shadows.'), nl,
    write('In the center sits a mysterious figure wrapped in a dark cloak.'), nl,
    write('His face is concealed by a violet mask that seems to shift in the light.'), nl,
    nl,
    write('The merchant seems hungry, eyeing you expectantly: "I could go for a proper meal..."'), nl.

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* current HP and inventory */
status :-
    hp(HP),
    write('Your HP: '), write(HP), nl,
    nl,
    show_inventory.

/* Display inventory */
show_inventory :-
    (   holding(_)
    ->  write('You are holding:'), nl,
        forall(holding(Item), (write('  - '), write(Item), nl))
    ;   write('Your hands are empty.'), nl
    ).

/* taking damage */
damage(Dmg) :-
    hp(HP),
    NewHP is HP - Dmg,
    write('You take damage: '), write(Dmg), nl,
    write('HP left: '), write(NewHP), nl,
    retract(hp(HP)),
    (   NewHP =< 0
    ->  assert(hp(0)),
        die
    ;   assert(hp(NewHP))
    ).

max_hp(3).

/* healing */
heal(Amount) :-
    hp(HP),
    max_hp(Max),
    NewHP is min(Max, HP + Amount),
    retract(hp(HP)),
    assert(hp(NewHP)),
    write('You reagain '), write(Amount), write(' HP.'), nl,
    write('Current HP: '), write(NewHP), nl.

/* This rule tells how to die. */

die :-
        write('You are dead!'), nl,
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.           -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('open(Object).            -- open the chest'), nl,
        write('attack(Target).          -- attack goblin / fire_elemental'), nl,
        write('bribe(goblin, Item).     -- bribe goblin with ANY item'), nl,
        write('fill_bucket.             -- fill bucket with water at the well'), nl,
        write('toss_coin.               -- toss a gold coin into the well'), nl,
        write('splash(fire_elemental).  -- splash water on the elemental'), nl,
        write('eat(Object).             -- a light Dungeon Meshi snack'), nl,
        write('drink(Object).           -- drink smth'), nl,
        write('brew_beer.               -- brew beer at the apparatus'), nl,
        write('press(Number).           -- press letter (1-24) on door in rune room'), nl,
        write('halt.                    -- to end the game and quit.'), nl,
        write('status.                  -- to see how much hp you have.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        retractall(hp(_)),
        assert(door_state(locked)),        
        assert(door_state_final(locked)),
        assert(hp(3)),
        instructions,
        status,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

% Opening the chest reveals its contents into the room
open(chest) :-
    i_am_at(room_b),
    chest_state(closed),
    retract(chest_state(closed)),
    assert(chest_state(open)),
    write('You open the chest.'), nl,
    % Wypuść przedmioty do pokoju
    forall(in(chest, Item), (retract(in(chest, Item)), assert(at(Item, room_b)))), !.

open(chest) :-
    i_am_at(room_b),
    chest_state(open),
    write('The chest is already open.'), nl, !.

open(wood_chest) :-
    i_am_at(storage_room),
    chest_state(closed2),
    retract(chest_state(closed2)),
    assert(chest_state(open2)),
    write('You open the wood chest.'), nl,
    forall(in(wood_chest, Item), (retract(in(wood_chest, Item)), assert(at(Item, storage_room)))), !.

open(wood_chest) :-
    i_am_at(storage_room),
    chest_state(open2),
    write('The chest is already open.'), nl, !.

open(locked_door) :-
    i_am_at(dwarfs_room),
    door_state(locked),
    (   dwarf_state(dead)
    ->  retract(door_state(locked)),
        assert(door_state(unlocked)),
        assert(path(dwarfs_room, e, rune_room)),
        write('You search the dwarf''s body and find the key.'), nl,
        write('The iron door unlocks with a heavy *CLUNK*.'), nl,
        !
    ;   dwarf_state(distracted)
    ->  retract(door_state(locked)),
        assert(door_state(unlocked)),
        assert(path(dwarfs_room, e, rune_room)),
        write('While the dwarf is distracted, you find the key on his belt.'), nl,
        write('You quietly unlock the iron door.'), nl,
        !
    ;   write('The dwarf blocks your path! "I said, NONE SHALL PASS!"'), nl,
        !
    ).

open(locked_door) :-
    i_am_at(dwarfs_room),
    door_state(unlocked),
    write('The door is already unlocked.'), nl.

open(_) :-
    write('You can''t open that!'), nl.

% Goblin: can be killed with sword OR bribed with any item
attack(goblin) :-
    i_am_at(room_a),
    goblin_state(alive),
    (   holding(sword)
    ->  retract(goblin_state(alive)),
        assert(goblin_state(dead)),
        retract(at(goblin, room_a)),
        write('You slay the goblin.'), nl,
        % bucket drops to the room, meat appears
        ( retract(held_by(bucket, goblin)) -> assert(at(bucket, room_a)) ; true ),
        assert(at(goblin_meat, room_a)),
        !
    ;   write('You have no weapon! The goblin punches you.'), nl,
        damage(1), !
    ).

attack(goblin) :-
    i_am_at(room_a),
    goblin_state(dead),
    write('The goblin is already dead.'), nl.

bribe(goblin, Item) :-
    i_am_at(room_a),
    goblin_state(alive),
    holding(Item),
    retract(holding(Item)),
    retract(goblin_state(alive)),
    assert(goblin_state(bribed)),
    retract(at(goblin, room_a)),
    write('The goblin is delighted and scampers off, dropping his bucket.'), nl,
    ( retract(held_by(bucket, goblin)) -> assert(at(bucket, room_a)) ; true ),
    !.

bribe(goblin, _) :-
    i_am_at(room_a),
    goblin_state(alive),
    \+ holding(_),
    write('You have nothing to bribe him with.'), nl.

% Dwarf: can be killed with sword OR bribed with beer
attack(dwarf) :-
    i_am_at(dwarfs_room),
    dwarf_state(alive),
    holding(sword),
    !,
    retract(holding(sword)),
    assert(at(broken_sword, dwarfs_room)), 
    write('You swing your sword at the dwarf with all your might!'), nl,
    write('*CLANG!* The sword shatters against the dwarf''s steel armor!'), nl,
    write('But the force of your blow staggers the dwarf...'), nl,
    write('As his armor rings from the impact, he loses balance and falls!'), nl,
    write('The dwarf crashes to the ground, defeated!'), nl,
    write('The dwarf drops his battle axe.'), nl,
    write('Shards of your broken sword scatter across the floor.'), nl,
    write('However, during the struggle he managed to wound you.'), nl,
    retract(dwarf_state(alive)),
    assert(dwarf_state(dead)),
    retract(at(dwarf, dwarfs_room)),
    ( retract(held_by(battle_axe, dwarf)) -> assert(at(battle_axe, dwarfs_room)) ; true ),
    assert(at(dwarf_meat, dwarfs_room)),
    damage(2),!.

attack(dwarf) :-
    i_am_at(dwarfs_room),
    dwarf_state(distracted),
    !,
    write('The dwarf is happily drinking your beer, completely unaware...'), nl,
    write('You strike him down from behind while he''s distracted!'), nl,
    write('He slumps over without ever knowing what hit him.'), nl,
    retract(dwarf_state(distracted)),
    assert(dwarf_state(dead)),
    retract(at(dwarf, dwarfs_room)),
    ( retract(held_by(battle_axe, dwarf)) -> assert(at(battle_axe, dwarfs_room)) ; true ),
    assert(at(dwarf_meat, dwarfs_room)),
    write('The dwarf drops his battle axe.'), nl,
    !.

attack(dwarf) :-
    i_am_at(dwarfs_room),
    dwarf_state(alive),
    \+ holding(sword),
    !,
    write('The dwarf laughs heartily! "Fool! You dare face me without a proper weapon?"'), nl,
    write('With one mighty swing of his battle axe, he cleaves you in two!'), nl,
    damage(3), 
    !.

attack(dwarf) :-
    i_am_at(dwarfs_room),
    dwarf_state(dead),
    write('The dwarf is already dead.'), nl.

bribe(dwarf, beer) :-
    i_am_at(dwarfs_room),
    dwarf_state(alive),
    holding(beer),
    !,
    retract(holding(beer)),
    retract(dwarf_state(alive)),
    assert(dwarf_state(distracted)),
    write('You offer the dwarf your freshly brewed beer.'), nl,
    write('His eyes light up! "Now that''s a proper drink!"'), nl,
    write('He eagerly takes the beer and starts drinking, completely forgetting about his duty.'), nl,
    ( retract(held_by(battle_axe, dwarf)) -> assert(at(battle_axe, dwarfs_room)) ; true ),
    write('In his merriment, he drops his battle axe.'), nl,
    !.

bribe(dwarf, _) :-
    i_am_at(dwarfs_room),
    dwarf_state(alive),
    holding(_),
    write('The dwarf scowls. "I don''t want that! Bring me proper dwarven drink!"'), nl,
    !.

bribe(merchant, special_meal) :-
    i_am_at(merchant_room),
    holding(special_meal),
    retract(holding(special_meal)),
    write('You offer the magical meal to the merchant.'), nl,
    write('He inhales the aroma and smiles: "Ah, finally some proper food!"'), nl,
    write('He waves his hand and a swirl of magical energy envelops you.'), nl,
    write('You are teleported out of the dungeon. Congratulations! You have won the game!'), nl,
    finish,
    !.

bribe(merchant, _) :-
    i_am_at(merchant_room),
    holding(_),
    write('The merchant sniffs the air: "I need a proper meal, not that!"'), nl,
    !.

bribe(_, _) :-
    write('That won''t work.'), nl.

attack(merchant) :-
    i_am_at(merchant_room),
    write('As you raise your weapon, the merchant doesn''t flinch.'), nl,
    write('The purple flames suddenly flare up!'), nl,
    write('A wave of unnatural force throws you against the wall!'), nl,
    write('The merchant remains seated, unmoving.'), nl,
    write('"Violence is the language of the simple-minded..."'), nl,
    damage(3),
    !.

% Well logic: fill bucket with water; toss coin for treasure
fill_bucket :-
    i_am_at(room_c),
    at(well, room_c),
    holding(bucket),
    \+ holding(water),
    assert(holding(water)),
    write('You fill the bucket with fresh well water.'), nl, !.

fill_bucket :-
    i_am_at(room_c),
    holding(water),
    write('Your bucket is already full.'), nl, !.

fill_bucket :- write('You need a bucket and to be at the well.'), nl.

toss_coin :-
    i_am_at(room_c),
    at(well, room_c),
    holding(gold_coin),
    retract(holding(gold_coin)),
    write('Plop! The coin vanishes. Writing on the wall appears...'), nl,
    write('“Meat, salt, and elemental ash, stirred and baked just right'), nl,
    write('A bite of this legendary dish brings flavors pure and bright.”'), nl,
    write('(Use cook_meal when you have the right ingedients.)'), nl, !.

cook_meal :-
    holding(cooled_ash),
    holding(salt),
    (holding(goblin_meat) ; holding(dwarf_meat) ; holding(mushrooms)),
    !,
    (   holding(goblin_meat) -> retract(holding(goblin_meat)) ; true ),
    (   holding(dwarf_meat) -> retract(holding(dwarf_meat)) ; true ),
    (   holding(mushrooms) -> retract(holding(mushrooms)) ; true ),
    retract(holding(cooled_ash)),
    retract(holding(salt)),
    assert(holding(special_meal)),
    write('You carefully cook a magical recipe combining the meat, salt, and the fire elemental ash.'), nl,
    write('The meal smells exquisite!'), nl.

cook_meal :-
    write('You do not have the right ingredients to cook the special meal.'), nl.


toss_coin :- write('No coin to toss, or you are not at the well.'), nl.

% Fire elemental: dies to water; sword melts if used on it
splash(fire_elemental) :-
    i_am_at(room_d),
    fire_elemental_state(alive),
    holding(water),
    retract(holding(water)), % woda zostaje zużyta
    retract(fire_elemental_state(alive)),
    assert(fire_elemental_state(dead)),
    retract(at(fire_elemental, room_d)),
    write('The elemental dissolves into a cloud of steam.'), nl,
    assert(at(cooled_ash, room_d)), !.

splash(fire_elemental) :-
    i_am_at(room_d),
    fire_elemental_state(alive),
    \+ holding(water),
    write('You have no water to splash!'), nl.

splash(fire_elemental) :-
    i_am_at(room_d),
    fire_elemental_state(dead),
    write('It is already dead.'), nl.

attack(fire_elemental) :-
    i_am_at(room_d),
    fire_elemental_state(alive),
    (   holding(sword)
    ->  retract(holding(sword)),
        write('You swing your sword, but it MELTS instantly in the intense heat!'), nl,
        write('The elemental lashes out, burning you.'), nl,
        damage(1), !
    ;   write('You have no weapon! You get too close and are burned.'), nl,
        damage(1), !
    ).

attack(fire_elemental) :-
    i_am_at(room_d),
    fire_elemental_state(dead),
    write('The elemental is just a pile of cool ash.'), nl.

attack(_) :-
    write('You can''t attack that!'), nl.


eat(goblin_meat) :-
    holding(goblin_meat),
    retract(holding(goblin_meat)),
    write('You cook and eat the goblin meat. Tastes... gamey. You feel a bit better.'), nl,
    heal(1), !.

eat(dwarf_meat) :-
    holding(dwarf_meat),
    retract(holding(dwarf_meat)),
    write('You butcher the dwarf and attempt to cook his meat...'), nl,
    write('This feels... wrong. Very wrong.'), nl,
    write('The meat smells foul and is saturated with dwarven ale.'), nl,
    write('You now have cooked dwarf meat. What have you become?'), nl,
    write('You take a bite of the dwarf meat...'), nl,
    write('It tastes even worse than it smells!'), nl,
    write('The meat is so saturated with alcohol it burns your throat.'), nl,
    write('You feel sick and dizzy from the foul concoction.'), nl,
    damage(1), !.

eat(dwarf_meat) :-
    write('You don''t have any cooked dwarf meat. Probably for the best.'), nl.

eat(mushrooms) :-
    holding(mushrooms),
    retract(holding(mushrooms)),
    write('You eat the glowing mushrooms...'), nl,
    write('The world starts to swirl around you!'), nl,
    write('Colors become brighter, sounds become distorted.'), nl,
    write('You feel euphoric but completely disoriented.'), nl,
    write('The runes on the walls seem to dance and whisper secrets...'), nl,
    nl,
    write('*** You are experiencing intense hallucinations ***'), nl,
    write('The inscription now reads: "beer is a blessing and a curSe"'), nl,
    write('The letters seem to float in the air...'), nl, !.

eat(special_meal) :-
    holding(special_meal),
    retract(holding(special_meal)),
    max_hp(Max),
    retractall(hp(_)),
    assert(hp(Max)),
    write('You eat the magical meal. Your body feels completely restored!'), nl,
    write('HP is now full.'), nl.


eat(_) :-
    write('You can''t eat that!'), nl.

drink(beer) :-
    holding(beer),
    retract(holding(beer)),
    write('You take a long drink of the freshly brewed beer.'), nl,
    write('It''s surprisingly good! You feel refreshed and a bit tipsy.'), nl,
    heal(2), !.

drink(_) :-
    write('You can''t drink that!'), nl.

brew_beer :-
    i_am_at(brewery_room),
    at(brewing_apparatus, brewery_room),
    holding(water),
    holding(malt), !,
    retract(holding(water)),
    retract(holding(malt)),
    assert(holding(beer)),
    write('You carefully combine water and malt in the brewing apparatus...'), nl,
    write('After some bubbling and hissing, you have crafted a fine beer!'), nl,
    nl,
    write('The beer looks refreshing and gives off a pleasant aroma.'), nl.

brew_beer :-
    i_am_at(brewery_room),
    \+ holding(water),
    write('You need water in a bucket to brew beer.'), nl, !.

brew_beer :-
    i_am_at(brewery_room),
    \+ holding(malt),
    write('You need malt to brew beer.'), nl, !.

brew_beer :-
    i_am_at(brewery_room),
    write('You need to be at the brewing apparatus with water and malt.'), nl.

press(Number) :-
    i_am_at(rune_room),
    door_state_final(locked),
    (   Number >= 1, Number =< 24
    ->  (   Number = 23
        ->  
            retract(door_state_final(locked)),
            assert(door_state_final(unlocked)),
            assert(path(rune_room, e, merchant_room)),
            write('*CLICK!* The door unlocks!'), nl,
            write('The inscription glows brightly: "The s in curse was the key!"'), nl,
            write('The door slowly swings open, revealing the final chamber beyond.'), nl;
            write('You press letter number '), write(Number), write('...'), nl,
            write('A magical shock courses through your body!'), nl,
            damage(1),
            write('Try another number.'), nl
        )
    ;   write('There are only 24 letters. Enter a number between 1 and 24.'), nl
    ), !.

press(Number) :-
    i_am_at(rune_room),
    door_state_final(unlocked),
    write('The door is already unlocked. You can proceed east.'), nl, !.

press(_) :-
    write('You can only press numbered letters in the rune room.'), nl.
