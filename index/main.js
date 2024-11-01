import * as Version from './version.js'

const get = async url => {
  const response = await fetch(url)
  return await response.json()
}

const init = async () => {
  const contents = await get(
    'https://api.github.com/repos/ocaml-multicore/picos/contents/?ref=gh-pages'
  )

  const versions = contents
    .filter(e => e.type === 'dir' && Version.is(e.name))
    .map(e => e.name)
    .sort(Version.compare)
    .reverse()

  const container = document.createElement('div')
  container.id = 'versions'
  document.getElementById('api').appendChild(container)

  for (const version of versions) {
    const a = document.createElement('a')
    a.href = version + '/'
    a.innerText = version

    container.appendChild(a)
  }
}

init()
