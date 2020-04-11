#!/bin/bash
# Config for Logitech G600 mouse using Ratbag

# BUTTONS

ratbagctl hollering-marmot button 7 action set special 'profile-cycle-up'

ratbagctl hollering-marmot button 8 action set macro +KEY_LEFTCTRL +KEY_LEFTSHIFT KEY_T -KEY_LEFTSHIFT -KEY_LEFTCTRL
ratbagctl hollering-marmot button 9 action set macro +KEY_LEFTCTRL KEY_W -KEY_LEFTCTRL
ratbagctl hollering-marmot button 10 action set macro +KEY_LEFTCTRL +KEY_LEFTSHIFT KEY_TAB -KEY_LEFTSHIFT -KEY_LEFTCTRL

ratbagctl hollering-marmot button 11 action set macro +KEY_LEFTCTRL KEY_C -KEY_LEFTCTRL
ratbagctl hollering-marmot button 12 action set macro +KEY_LEFTCTRL KEY_V -KEY_LEFTCTRL
ratbagctl hollering-marmot button 13 action set macro +KEY_LEFTCTRL KEY_TAB -KEY_LEFTCTRL

ratbagctl hollering-marmot button 14 action set macro +KEY_LEFTCTRL KEY_A -KEY_LEFTCTRL
ratbagctl hollering-marmot button 15 action set macro +KEY_LEFTCTRL KEY_V -KEY_LEFTCTRL
ratbagctl hollering-marmot button 16 action set macro KEY_F5

ratbagctl hollering-marmot button 17 action set macro +KEY_LEFTCTRL KEY_C -KEY_LEFTCTRL
ratbagctl hollering-marmot button 18 action set macro +KEY_LEFTALT KEY_TAB -KEY_LEFTALT
ratbagctl hollering-marmot  button 19 action set macro KEY_ENTER

# ratbagctl cheering-viscacha button 19 action set macro KEY_ENTER


# LEDS

ratbagctl hollering-marmot led 0 set duration 20000
ratbagctl hollering-marmot led 0 set mode breathing
ratbagctl hollering-marmot led 0 set color 670067

# aa0010 - cool red
