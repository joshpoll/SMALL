# SMALL

**SMALL** is an abstract machine that executes SML programs. It relies on the
(HaMLet)[https://github.com/rossberg/hamlet] reference interpreter's parser. It's implementation
follows that of HaMLet closely, except HaMLet is an interpreter and **SMALL** is an abstract
machine.

**SMALL** also contains a high-fidelity debugger that proceeds rule-by-rule. This debugger produces
the entire state of **SMALL**'s abstract machine as well as information about how data flows from
one step to the next.

# Initialize and Run (TODO: improve)
```sh
git clone --recurse-submodules git@github.com:joshpoll/theia.git
cd src/server/hamlet
make # using your favorite SML implementation. We recommend Poly/ML: https://www.polyml.org/index.html
cd -
npm install
npm run server
# in a new tab
npm start
# in a new tab
cd src/server
python3 server.py # requires Python 3.7. TODO: This is probably missing some dependencies right now.
```

# Build
```
npm run build
```

# Build + Watch

```
npm run start
```

# Editor
If you use `vscode`, Press `Windows + Shift + B` it will build automatically


# What **SMALL** Supports

## Rules
- [x] 90
- [x] 91
- [x] 92
- [x] 93
- [x] 94
- [x] 95
- [x] 96
- [x] 97
- [ ] 98
- [ ] 99
- [ ] 100
- [x] 101
- [x] 102
- [ ] 103
- [ ] 104
- [ ] 105
- [ ] 106
- [ ] 107
- [x] 108
- [x] 109
- [x] 110
- [x] 111
- [x] 112
- [x] 113
- [x] 114
- [ ] 115
- [ ] 116
- [ ] 117
- [ ] 118
- [ ] 119
- [ ] 120
- [ ] 121
- [ ] 122
- [ ] 123
- [ ] 124
- [ ] 125
- [ ] 126
- [ ] 127
- [ ] 128
- [ ] 129
- [ ] 130
- [ ] 131
- [x] 132
- [ ] 133
- [ ] 134
- [ ] 135
- [ ] 136
- [ ] 137
- [x] 138
- [x] 139
- [x] 140
- [x] 141
- [x] 142
- [x] 143
- [x] 144
- [x] 145
- [ ] 146
- [ ] 147
- [ ] 148
- [ ] 149
- [ ] 150
- [ ] 151
- [ ] 152
- [ ] 153
- [ ] 154
- [ ] 155
- [x] 156
- [ ] 157
- [ ] 158
- [ ] 159
- [x] 160
- [ ] 161
- [ ] 162
- [ ] 163
- [ ] 164
- [ ] 165
- [ ] 166
- [ ] 167
- [ ] 168
- [ ] 169
- [ ] 170
- [ ] 171
- [ ] 172
- [ ] 173
- [ ] 174
- [ ] 175
- [ ] 176
- [ ] 177
- [ ] 178
- [ ] 179
- [ ] 180
- [ ] 181
- [ ] 182
- [ ] 183
- [ ] 184
- [ ] 185
- [ ] 186
- [ ] 187
- [ ] 188
- [ ] 189

# What **SMALL** Doesn't Support
We currently don't have plans to support type-checking or good error messages.
