#let article(
  title: "algún título",
  author: "algún autor",
  paper: "a4",
  cliente: "algún cliente",
  nro_muestra: "algún nro de muestra",
  muestra: "alguna muestra",
  fecha_muestra: "alguna fecha muestra",
  ensayos: "algún ensayo",
  observaciones: "alguna observación",
  body,
) = {
  set document(title: title, author: author)

  set text(font: "Times New Roman")

  set heading(numbering: "1.1.")

  set page(
    paper: paper,
    fill: rgb("ffffff"),
    margin: (bottom: 2cm, top: 2.5cm, left: 2cm, right: 2cm),
    footer: align(center, link("https://vhgauto-seminario4-gulich.share.connect.posit.cloud/")),
    numbering: "1/1",
    header: [#grid(
      columns: (1fr, 1fr, 1fr),
      gutter: 1pt,
      align: (center + horizon),
      [#figure(
        image("www/logo_ig.png", width: 40%)
      )],
      [#text(size: 2em, fill: rgb("#00008b"))[
        Informe Paraná
      ]],
      [#text(size: 1em)[
        *Víctor Gauto - Seminario 4 \
        Instituto Gulich*
      ]]
    )
    #line(length: 100%)]
  )

  page(align(left + top)[
    #body
  ])
}
