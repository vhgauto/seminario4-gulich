#show: article.with(
$if(title)$
  title: "$title$",
$endif$

$if(author)$
  author: "$author$",
$endif$

$if(params.nro_muestra)$
  nro_muestra: "$params.nro_muestra$",
$endif$

$if(params.cliente)$
  cliente: "$params.cliente$",
$endif$

$if(params.muestra)$
  muestra: "$params.muestra$",
$endif$

$if(params.fecha_muestra)$
  fecha_muestra: "$params.fecha_muestra$",
$endif$

$if(params.ensayos)$
  ensayos: "$params.ensayos$",
$endif$

$if(params.observaciones)$
  observaciones: "$params.observaciones$",
$endif$

)
