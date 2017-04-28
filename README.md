# Gameplay

Move the ship with the left and right arrow keys. Press Space to fire.
Additional controls (e.g. navigation keys) are documented on-screen.

# Implementation

To reduce code duplication, behavior for elements whose locations need to
be tracked and whose elements need to be rendered on-screen is pulled out
into a Sprites module.

The score is a simple measure of the number of enemies killed. Levels advance
as a function of the score, and the probability of an enemy leaving formation
increases with the level, as does their probability of firing a bullet. In
this way, the game gets more difficult as the user kills more enemies.
Because the difficulty is generated dynamically, there is no upper limit on
the number of levels (however, for ease of grading, the difficulty is set
to increase fairly aggressively).

Bullets are cleared as they leave the screen to avoid performance issues from
rendering many off-screen elements that have no longer have any bearing on
the game.

# Code Style/Design

Where possible, functions are written to be applied to singular data structures
(e.g. collision detection simply detects a collision between two sprites, rather
than trying to compare one sprite with list of potential dangers), and are
simply applied in functor/applicative style or in a zipping context if
evaluation against a list is necessary. This makes them more reusable and less
context-dependent.

The use of Maybes is preferred over passing of in-band error or sentinel
values, and is also preferred over if/then branching. For instance, the
function to produce an enemy-fired bullet returns a Maybe Bullet that can be
Nothing if the enemy has not left formation or if the angle of the bullet would
be undesirable.

Lines of code are capped at 80 characters for readability. A brief line of
documentation is provided for each function.

# Features

## Explosions
Enemies and ships explode when hit by a bullet. When the animation is complete,
the explosion sprite is cleared from the screen.

## Bezier curve trajectories for enemies
Enemies follow a Bezier curve to the bottom of the screen. If they reach
the bottom unscathed, then they are placed back into formation at the top
of the screen. To ensure that they can be seamlessly reinserted, open spots
in the formation are tracked and updated as the formation moves about.
Enemies are assigned one of the defined paths at random to ensure variety of
movement. Enemies store a data type called RogueInfo to track information about
their trajectories. For example, the enemy's location when they first "go rogue"
is stored so that the coordinates provided by the curve function can be treated
as offsets. Additional implementation notes provided inline.

## Variable bullet trajectories
Enemies don't fire straight down, but rather at an angle that is calculated
based on their current trajectory. To ensure that bullets at different angles
travel at the same speed, the (deltaX, deltaY) vector is normalized, and
then multiplied a constant "bullet speed" scalar. Outlandish trajectories
(e.g. a mostly-horizontal path calculated from an enemy that happens to be
moving horizontally) are filtered out.

# Misc

A makefile is provided to clear out compilation artifacts.

  - `make` to compile
  - `make clean` to remove the executable, *.o, and *.hi files.
