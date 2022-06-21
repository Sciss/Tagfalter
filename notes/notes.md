# Notes 19-Jun-2022

- [X] crypsis: envelope ; duration based on spatial positions
- [X] space-timbre amplitude could vary between a min/max range (as low as 6 dB)
- [X] place gap after biphase-send before recording
- [X] space-timbre: skip communication frequencies
- [X] accel-play: should we use compression / limiter?
- [X] detect-space, only run at a maximum frequency, e.g. no more than once every ten minutes
- [X] add another silent stage, that is entered at random times

# Notes 21-Jun-2022

- [X] after space-pos update: send individual frequencies (1 byte id, 2x2 bytes frequencies)
- [X] at random intervals: communicate on individual frequencies *joyous* (others respond max. once)
- [ ] continuously run biphase-rcv ? then release any process when detecting global comm
- [ ] after a period (e.g. hour), re-start accel-rec based on current spatial positions
- [ ] accel: filter communication frequencies?
- [ ] biphase-send clicks at end (need to wait for fade-out of lag)
- [ ] joy: add pause to "wait for responses"
- [ ] might be good to randomise the inter-space waiting duration, so chances are low that multiple nodes re-run
      space-detect at the same time
