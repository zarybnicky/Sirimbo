import * as React from 'react';
import { Container, Grid, Typography } from '@mui/material';
import { Hero } from 'components/Hero';
import { ServiceCard } from 'components/cards/ServiceCard';
import { CallToAction } from 'components/CallToAction';
import { ArticleCard } from 'components/cards/ArticleCard';
import { VideoCard } from 'components/cards/VideoCard';
import { useTitleVideos } from 'lib/data/use-videos';
import { useArticles } from 'lib/data/use-articles';
import { useServices } from 'lib/data';
import { NextLinkComposed } from 'components/Link';
import { Carousel as BsCarousel } from 'react-bootstrap';
import { FooterMap } from 'components/Footer';
import { useConfig } from 'lib/use-config';

export const HomePage = ({ }) => {
  const { articles } = useArticles(2, 3);
  const videos = useTitleVideos();
  const services = useServices();

  const { layout } = useConfig();
  if (!layout || layout === 'old') {
    return <OldHomePage />
  }

  return <>
    <Hero />
    <Container maxWidth="lg">
      {services.map((x, i) => (
        <ServiceCard key={i} image={x.image} header={x.header}>
          <Typography variant="body1">{x.text}</Typography>
        </ServiceCard>
      ))}
    </Container>

    <CallToAction />

    <Container maxWidth="lg" style={{ margin: '3rem auto' }}>
      <Grid container spacing={3}>

        <Grid item sm={12} md={6}>
          <Typography gutterBottom variant="h4" component="h2">Aktuálně</Typography>
          <Grid container spacing={3} style={{ alignItems: "stretch" }}>
            {articles.map((x, i) => <Grid item container sm={12} md={6} key={i}><ArticleCard item={x} /></Grid>)}
          </Grid>
        </Grid>

        <Grid item sm={12} md={6}>
          <Typography gutterBottom variant="h4" component="h2">Videa</Typography>
          <Grid container spacing={3}>
            {videos.map((x, i) => <Grid item sm={12} key={i}><VideoCard item={x} /></Grid>)}
          </Grid>
        </Grid>

      </Grid>
    </Container>
  </>;
}

export default HomePage;


const OldHomePage = () => {
  const { articles } = useArticles(2, 3);
  const videos = useTitleVideos();
  const { articles: highlights } = useArticles(3, 0);

  return <>
    <BsCarousel>
      {highlights.map((item, i) => (
        <BsCarousel.Item key={i}>
          <img src={item.img} alt={item.header} style={{
            display: 'block',
            height: '100%',
            width: '100%',
            objectFit: 'cover',
            objectPosition: '50% 30%',
            backgroundImage: 'linear-gradient(to bottom, rgba(0, 0, 0, 0.1), rgba(0, 0, 0, 0.4))'
          }} />
          <BsCarousel.Caption>
            <NextLinkComposed className="color-white" href={item.href}>
              <h3>{item.header}</h3>
            </NextLinkComposed>
            <p className="d-none d-md-block">{item.preview}</p>
          </BsCarousel.Caption>
        </BsCarousel.Item>
      ))}
    </BsCarousel>

    <div className="container about" style={{ marginTop: '3.5rem', marginBottom: '3.5rem' }}>
      <div className="row">
        <div className="col-md-6">
          <h2>O klubu</h2>
          <p>
            <b>TK Olymp</b> je tradiční taneční klub zabývající se výchovou
            standardních a&nbsp;latinskoamerických tanečníků. V&nbsp;novém tisíciletí
            patří mezi&nbsp;nejúspěšnější české kluby. Naše páry pravidelně sbírají
            úspěchy v&nbsp;České republice i&nbsp;na&nbsp;mezinárodním poli.
          </p>
          <p>
            Nabízíme atraktivní volnočasovou aktivitu dětem, kvalitní zázemí
            sportovním tanečníkům a&nbsp;příjemné prostředí pro&nbsp;všechny, co&nbsp;mají
            rádi tanec.
          </p>
        </div>
        <div className="col-md-6 text-center">
          <img className="logo-image" alt="" src="/style/logo-big.png" />
        </div>
      </div>
    </div>
    <div className="courses">
      <div>
        <div className="container">
          <div className="row">
            <div className="col-md-6 courses__desc text-right">
              <h4>Taneční přípravka (5&ndash;7&nbsp;let)</h4>
              <p>
                Taneční a&nbsp;pohybové hry, seznámení s&nbsp;hudbou a&nbsp;základními kroky.
              </p>
              <p>
                Vyučuje mladý proškolený trénérský kolektiv v&nbsp;kombinaci se&nbsp;zkušenými&nbsp;učiteli.
              </p>
            </div>
            <div className="col-md-6 courses__img">
              <img alt="" src="/images/julca-mikulas.jpg" />
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="container">
          <div className="row">
            <div className="col-md-6 courses__img text-right">
              <img alt="" src="/images/boruvci.jpg" />
            </div>
            <div className="col-md-6 courses__desc">
              <h4>Tančení pro&nbsp;děti (8&ndash;14&nbsp;let)</h4>
              <p>
                Pohybová a&nbsp;taneční průprava, základní figury a&nbsp;techniky standardních
                a&nbsp;latinskoamerických tanců.
              </p>
              <p>
                Možné jako kroužek i&nbsp;jako příprava na&nbsp;taneční sport.
              </p>
              <p>
                Vyučuje mladý proškolený trénérský kolektiv, složený z&nbsp;mistrů České
                republiky a&nbsp;tanečníků mezinárodní třídy.
              </p>
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="container">
          <div className="row">
            <div className="col-md-6 courses__desc text-right">
              <h4>Lekce pro&nbsp;začátečníky (15+&nbsp;let)</h4>
              <p>
                Vhodné pro všechny, kteří ještě netančili nebo prošli kurzy v&nbsp;tanečních.
              </p>
              <p>
                Vyučujeme základní figury a techniky standardních a
                latinskoamerických tanců, držení těla, souhru s&nbsp;partnerem a&nbsp;soulad
                s&nbsp;hudbou.
              </p>
              <p>
                Vyučuje mladý proškolený trénérský kolektiv, složený z&nbsp;mistrů České republiky
                a&nbsp;tanečníků mezinárodní třídy.
              </p>
            </div>
            <div className="col-md-6 courses__img">
              <img alt="" src="/images/david-verca.jpg" />
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="container">
          <div className="row">
            <div className="col-md-6 courses__img text-right">
              <img alt="" src="/images/vilda-lat.jpg" />
            </div>
            <div className="col-md-6 courses__desc">
              <h4>Taneční sport</h4>
              <p>
                Vhodné pro&nbsp;pokročilé tanečníky se&nbsp;soutěžními ambicemi od&nbsp;dětí
                po&nbsp;dospělé.
              </p>
              <p>
                Tréninkový plán je zaměřený na výuku techniky standardních a&nbsp;latinskoamerických
                tanců, kondiční a&nbsp;fyzickou přípravu a&nbsp;soutěžní podání tanečního
                sportu.
              </p>
              <p>
                Vyučují extérní lektoři z&nbsp;Prahy, Brna, Ostravy - trenéři nejlepších párů
                a&nbsp;mistrů České republiky.
              </p>
              <p>
                Volné tréninky pro&nbsp;vlastní samostatné trénování, možnost individuálních
                lekcí s&nbsp;extérními trenéry.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>

    <div className="container news">
      <div className="row" style={{ marginTop: '3.5rem', marginBottom: '3.5rem' }}>
        <div className="col-md-6">
          <h3>
            <NextLinkComposed href="/aktualne">Aktuálně</NextLinkComposed>
          </h3>
          <div className="row">
            {articles.map((item, i) => (
              <div className="col-12 col-md-6 pb-2" key={i}>
                <div className="article-small card">
                  <a className="card-img-top image" href={item.href}>
                    <img alt="" src={item.imgThumb} />
                  </a>
                  <div className="card-body">
                    <h4 className="card-title">
                      <NextLinkComposed href={item.href}>{item.header}</NextLinkComposed>
                    </h4>
                    <p className="card-text">{item.preview}</p>
                    <div style={{ textAlign: 'right', fontSize: '85%' }}>
                      <NextLinkComposed href={item.href}>více zde &raquo;</NextLinkComposed>
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>

        <div className="col-md-6">
          <h3>
            <NextLinkComposed href="/video">Videa ze soutěží</NextLinkComposed>
          </h3>
          <ul className="list-group">
            {videos.map((x, i) => (
              <li className="list-group-item" key={i}>
                <NextLinkComposed className="video row" href={x.href}>
                  <div className="col-4"><img style={{ maxWidth: '100%' }} alt="" src={x.img} /></div>
                  <div className="col-8"><h5>{x.name}</h5></div>
                </NextLinkComposed>
              </li>
            ))}
          </ul>
        </div>
      </div>
    </div>

    <div className="container contact">
      <h2>Kontakt</h2>
      <div className="row">
        <div className="col-md">
          <h3>Taneční klub</h3>
          <p>
            <b>Taneční klub Olymp Olomouc</b><br />
            Jiráskova 25, 779 00 Olomouc<br />
            IČO: 68347286<br />
            <a href="mailto:miroslav.hyza@tkolymp.cz">miroslav.hyza@tkolymp.cz</a>
          </p>
          <h3>Taneční sály</h3>
          <p>
            <b>Taneční centrum při FZŠ Holečkova</b><br />
            Holečkova 10, 779 00 Olomouc<br />
            (vchod brankou u&nbsp;zastávky <i>Povel, škola</i>)
          </p>
          <p>
            <b>Tělocvična Slovanského gymnázia</b><br />
            Jiřího z Poděbrad 13, 779 00 Olomouc<br />
            (vchod bránou z&nbsp;ulice <i>U reálky</i>)
          </p>
        </div>
        <div className="col-md">
          <FooterMap height='400px' />
        </div>
      </div>
    </div>
  </>;
};
