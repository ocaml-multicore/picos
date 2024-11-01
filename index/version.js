export const is = s => !!s.match(/^[0-9]+\.[0-9]+\.[0-9]+$/)

export const parse = s => s.split('.').map(n => parseInt(n))

export const compare = (l, r) => {
  l = parse(l)
  r = parse(r)
  for (let i = 0; i < 3; ++i) {
    if (l[i] < r[i]) return -1
    if (r[i] < l[i]) return 1
  }
  return 0
}
