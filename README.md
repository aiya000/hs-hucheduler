# :diamond_shape_with_a_dot_inside: hs-hucheduler :diamond_shape_with_a_dot_inside:
:bulb: Simple weekly schedule viewer ! :bulb:


# :key: How to prepare :key:
1. `$ mkdir ~/.config/hucheduler`
2. `$ cp ./schedules-initial ~/.config/hucheduler/schedules`
3. `$ mkdir -p ~/.cache/hucheduler/daily`
4. `$ $EDOTOR ~/.config/hucheduler/schedules`
    - Modify it for your schedules
    - Please refer to ./schedules-example
5. `$ stack install`
    - `'hucheduler' will be installed to your ~/.local/bin`


# :gift: How to use :gift:
1. `$ hucheduler`
    - :confetti_ball: Your schedule of today will be shown
2. hucheduler confirms 'Do you remember reading the schedule ? (y/n)'
    - :no_bell: The schedule is never shown at today if you answer 'y'
    - :bell: Please remove `~/.cache/hucheduler/daily/{today}_is-cheched` file if you want to view today's schedule once more


# :relaxed: In my use case :relaxed:
I'm writing below line in my ~/.zshrc

```sh
.
.
(some line)
.
.
hucheduler
```


# :grey_exclamation: Requirements :grey_exclamation:
- `stack` (haskell-stack)
