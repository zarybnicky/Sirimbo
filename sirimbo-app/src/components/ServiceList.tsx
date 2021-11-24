import * as React from 'react';
import { Container, Paper } from '@material-ui/core';

// Red/black separator + header text, spacing
export const ServiceList = () => <Container maxWidth="lg">
  <Paper elevation={3}>
    Matýsek
    <h2>Přípravka tanečního sportu</h2>
    <p>
      První kroky do světa tanečního sportu pro děti od 5 do 10 let. Všeobecná
      taneční průprava a základy tanečních kroků pro budoucí hvězdy
    </p>
  </Paper>

  <Paper elevation={3}>
    Malí
    <h2>Základy tanečního sportu</h2>
    <p>
      Tréninkové programy pro začínající a mírně pokročilé tanečníky ve věkových
      skupinách juniorů (12-15 let), mládež a dospělí (16+ let).
    </p>
  </Paper>

  <Paper elevation={3}>
    Honová
    <h2>Výkonnostní sport</h2>
    <p>
      Tréninkové programy pro soutěžní tanečníky ve všech věkových skupinách a
      výkonnostních třídách uzpůsobené podle potřeb v jednotlivých výkonnostních
      stupních.
    </p>
  </Paper>

  <Paper elevation={3}>
    Šír
    <h2>Sportovní centrum mládeže</h2>
    <p>
      Tréninkový program pro vrcholové sportovce, reprezentanty ČR se špičkovými
      českými trenéry, speciální kondiční přípravou a moderními metodami
      sportovního tréninku. Jsme jediným klubem v Olomouckém kraji se statutem
      Sprtovního centra mládeže dle MŠMT.
    </p>
  </Paper>
</Container>
