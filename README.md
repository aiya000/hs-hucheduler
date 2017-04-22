# hs-hucheduler :diamond_shape_with_a_dot_inside:
　Simple weekly schedule viewer ! :bulb:


# How to prepare
1. `$ mkdir ~/.config/hucheduler`
2. `$ cp ./schedules-initial ~/.config/hucheduler/schedules`
3. `$ mkdir -p ~/.cache/hucheduler/daily`
4. `$ $EDOTOR ~/.config/hucheduler/schedules`
    - Modify it for your schedules :+1:
    - Please refer to ./schedules-example
5. `$ stack install`
    - `'hucheduler' will be installed to your ~/.local/bin`


# How to use
1. `$ hucheduler`
    - Your schedule of today will be shown
2. hucheduler confirms 'Do you remember reading the schedule ? (y/n)'
    - The schedule is never shown at today if you answer 'y'
    - Please remove `~/.cache/hucheduler/daily/{today}_is-cheched` file if you want to view today's schedule once more


# In my use case
I'm writing below line in my ~/.zshrc

```sh
.
.
(some line)
.
.
hucheduler
```


# Requirements
- `stack` (haskell-stack)
