
A note on the obj toolkit

Essentially, this is a stripped-down version of gen_serv, and I
apologize for reinventing the wheel.  There is also serv:bc, which is
a stripped down version of gen_event.  The difference essentially
boils down to a preference for using functions over modules, i.e. I do
not want to create a module for each server object or event hub.
Maybe that is a mistake?  I'm not sure.

