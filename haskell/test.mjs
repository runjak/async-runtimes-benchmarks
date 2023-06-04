// npx zx test.js
// https://github.com/google/zx

const variants = ['chan1', 'mvar', 'ioref'] // ['chan1', 'chan2', 'sem', 'mvar', 'ioref']
const durations = [1, 1000, 10000]

const compileThreaded = () => $`ghc -O2 -prof -threaded -rtsopts -with-rtsopts=-N -o Main MainV7.hs`

const compileProfiled = () => $`ghc -O2 -o Main -prof MainV7.hs`

const runProfiled = async (variant, duration, prefix) => {
  console.log(`runProfiled: ${variant} 1000000 ${duration} (${prefix})`)
  try {
    await $`time ./Main ${variant} 1000000 ${duration} +RTS -hc -p`
    await $`hp2ps Main.hp`
    const fileName = `${prefix}_${variant}_1000000_${duration}`
    await $`mv Main.prof ${fileName}.prof`
    await $`gs -dSAFER -dBATCH -dNOPAUSE -dEPSCrop -r300 -sDEVICE=pngalpha -sOutputFile=${fileName}.png Main.ps`
    await $`convert ${fileName}.png -rotate -90 ${fileName}.jpg`
    await $`rm Main.ps Main.hp ${fileName}.png`
  } catch {
    console.log('broken.')
  }
}

const compileSimple = () => $`ghc -O2 -o Main MainV7.hs`

const runSimple = async (variant, duration) => {
  console.log(`runSimple: ${variant} 1000000 ${duration}`)
  await $`/usr/bin/time -f "%M" ./Main ${variant} 1000000 ${duration} &> ${variant}_1000000_${duration}.time`
}

const main = async () => {
  await compileThreaded()

  for (const variant of variants) {
    for (const duration of durations) {
      await runProfiled(variant, duration, 'threaded')
    }
  }

  await compileProfiled()

  for (const variant of variants) {
    for (const duration of durations) {
      await runProfiled(variant, duration, 'singleCore')
    }
  }

  await compileSimple()

  for (const variant of variants) {
    for (const duration of durations) {
      await runSimple(variant, duration)
    }
  }
}

await main()