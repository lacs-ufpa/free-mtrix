def cummulative_effect(DecreaseValue, ReserveItems):
    LReadjustPorcentage = 3
    Items = ReserveItems - DecreaseValue
    print('Old:', Items)

    LItemsToIncrement = (LReadjustPorcentage*Items) // 100
    Items = Items + LItemsToIncrement
    print('New:', Items)
    print('')
    return Items

ReserveItems = 200

for _ in range(10):
    ReserveItems = cummulative_effect(9, ReserveItems)

# cummulative_effect(DecreaseValue=3, ReserveItems)
# Old: 197
# New: 202

# Old: 199
# New: 204

# Old: 201
# New: 207

# Old: 204
# New: 210

# Old: 207
# New: 213

# Old: 210
# New: 216

# Old: 213
# New: 219

# Old: 216
# New: 222

# Old: 219
# New: 225

# Old: 222
# New: 228


# cummulative_effect(DecreaseValue=9, ReserveItems)
# Old: 191
# New: 196

# Old: 187
# New: 192

# Old: 183
# New: 188

# Old: 179
# New: 184

# Old: 175
# New: 180

# Old: 171
# New: 176

# Old: 167
# New: 172

# Old: 163
# New: 167

# Old: 158
# New: 162

# Old: 153
# New: 157