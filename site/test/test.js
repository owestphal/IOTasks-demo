import 'expect-puppeteer'

beforeAll(async () => {
  await page.setViewport({width: 1920, height: 1080})
  await page.goto("http://localhost:1234")
})

beforeEach(async () => {
  await page.reload()
})

function compileExample(example,random=false){
  let f = async () => {
    try {
      await expect(page).toClick("button", {text: example})
      if (random) {await expect(page).toClick("button", {text: "Random"})}
      await expect(page).toClick("button", {text: "Compile"})
      await expect(page).toMatchTextContent("Compiled successfully.", {timeout: 2000})
      if (random) {await expect(page).toClick("button", {text: "Constraint-based"})}
    } catch (e) {
      await page.screenshot({path: example+'-fail.png'})
      throw e
    }
  }
  return f
}

describe("Example compilation", () => {
  ["Sum"
  ,"Sum, with optional output"
  ,"Sum to zero"
  ,"Product (potential for overflows)"
  ,"Single path"
  ,"Exponentially many sat. paths"
  ,"Handling string output"
  ,"Basic setup"
  ].forEach((example) => it('should work for "'+example+'"', compileExample(example)))

  it('should work for "Sum" with random inputs',compileExample("Sum", true))

})
