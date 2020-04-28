A note on the WebSocket toolkit

One thing that the proliferation of web frameworks tells me, is that
there is no proper way to approach the problem.  The ws code in this
repository was grown to support embedded software applications.  The
code consists of what I consider to be bare essentials for distrubuted
application development: some way for a collecting of things on one
side, to talk to a collection of things on the other.  The
implementation consists of:

1) an object + communication model that matches both sides well.  For
Erlang this is processes + messages.  For the Javascript side this is
implemented using an object model based on mixins: an object is a DOM
node carrying a references to a list of mixins that determine its
behavior.

2) a hierarchy model that can be used to create single page
applications out of a collection of smaller, encapsulated widgets.
This is implemented as a dispatch mechanism over the WebSocket, that
allows processes on the Erlang side to talk directly to specific DOM
nodes augmented with mixin behavior.

3) a serialization/deserialization protocol based on Erlang terms
extended with some types corresponding to typical HTML input elements.
JSON is still supported, but a transition is being made to use binary
erlang terms at the lowest levels.

4) the glue code necessary to start up this structure as a 2-phase
mechanism. Phase 1 being initial layout rendering and phase 2 being
message communication between Erlang processes and JavaScript code.

Anything else is left up to the user.  Especially the rendering of
XHTML is not abstracted beyond some ad-hoc Erlang functions that
factor out common patterns, and the use of Erlang terms to represent
XHTML.

Note that this all is very ad-hoc, and has a backwards compatibility
constraint w.r.t. the applications written on top of it.  However, a
structure seems to be emerging that would allow for a simpler "version
2" to be written with a more deliberate design.  For now this is not a
priority.

