purescript-abc2psom
===================

Generate a purescript-school-of-music (PSoM) melody from ABC input.  The main idea is that a PSoM player handles polyphony, using multiple different MIDI instruments whereas a simple ABC player (from abc-melody) is monophonic.

This code has been hived off from the halogen editor in the PSoM project.  This allows for a library module to be built and also for more straightforward testing which in turn means that the way in which a PSoMProgram is constructed is clarified.

to build
--------

    spago install
    spago build

To test
-------

    npm run test