import * as React from "react";
import { AppProps, NextWebVitalsMetric } from 'next/app';
import Router, { useRouter } from "next/router";
import NProgress from "nprogress";
import "nprogress/nprogress.css";
import { GoogleAnalytics, event } from "nextjs-google-analytics";
import { ApolloProvider } from '@apollo/client';
import { client } from "lib/apollo";
import Head from 'next/head'
import { Header } from 'components/Header';
import { Footer } from 'components/Footer';
import { ProvideAuth, useAuth } from 'lib/data/use-auth';
import { ThemeProvider } from "@mui/material";
import { theme } from "lib/theme";
import 'bootstrap/dist/css/bootstrap.min.css';
import "public/style/index.scss";
import 'public/style/material-icons.css';
import '@react-page/editor/lib/index.css';
import '@react-page/plugins-slate/lib/index.css';
import '@react-page/plugins-image/lib/index.css';
import { SnackbarProvider } from "notistack";
import { LocalizationProvider } from "@mui/x-date-pickers/LocalizationProvider";
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns';
import { useCookie } from "lib/use-cookie";
import { Col, Container, Dropdown, Row } from 'react-bootstrap';
import LoginPage from "./login";
import { LoginForm } from "components/LoginForm";

Router.events.on("routeChangeStart", () => NProgress.start());
Router.events.on("routeChangeComplete", () => NProgress.done());
Router.events.on("routeChangeError", () => NProgress.done());

export default function MyApp({ Component, pageProps }: AppProps) {
  const [layout, setLayout] = useCookie('layout', 'old');
  const Layout = layout === 'old' ? OldLayout : NewLayout;
  const router = useRouter()

  React.useEffect(() => {
    import('react-facebook-pixel')
      .then((x) => x.default)
      .then((ReactPixel) => {
        ReactPixel.init('704526480597551');
        ReactPixel.pageView();
        router.events.on('routeChangeComplete', () => ReactPixel.pageView());
      })
  }, [router.events]);

  return (
    <ThemeProvider theme={theme}>
      <LocalizationProvider dateAdapter={AdapterDateFns}>
        <ApolloProvider client={client}>
          <SnackbarProvider maxSnack={3}>
            <ProvideAuth>
              <Head>
                <meta charSet="utf-8" />
                <meta name="viewport" content="initial-scale=1.0, width=device-width" />
                <meta name="keywords" content="taneční klub, tk, olymp, olomouc, tk olymp, sportovní tanec" />
                <meta name="ICBM" content="49.591700,17.285174" />
                <meta name="geo.placename" content="Olomouc, Česká Republika" />
                <meta name="geo.position" content="49.591700;17.285174" />
                <meta name="geo.region" content="cs" />
                <meta name="wot-verification" content="ec0cf41ab42dae52d3d4" />
                <meta name="msvalidate.01" content="7BD6C8B5748FC22EF06AB3AE89900885" />
                <meta name="facebook-domain-verification" content="k8tt64a93roxiymxo79clpvklan9j2" />
                <meta name="google-site-verification" content="Hfe7zlgTDOIpJv4rKGQz2Xg8Aezb6sIO0aAxVhrml9w" />
                <meta name="norton-safeweb-site-verification" content="r44xj2vskhlgkyqcqm1hdgga2jdfj-idvyys0277y96s72k-tq0z-yyjdu7h3el6pi2gek0i4ykq3xgiguufrvuhj8nbj4n4miwjhvumhp35jfrafyynhj4ee8ctzpzh" />
                <meta property="fb:app_id" content="132983570203245" />
                <link rel="shortcut icon" type="image/x-icon" href="/favicon.ico" />
              </Head>
              <GoogleAnalytics gaMeasurementId="UA-44456908-1" trackPageViews={{ ignoreHashChange: true }} />
              <Layout>
                <Component {...pageProps} />
              </Layout>
            </ProvideAuth>
          </SnackbarProvider>
        </ApolloProvider>
      </LocalizationProvider>
    </ThemeProvider>
  );
}

export function reportWebVitals({ id, name, label, value }: NextWebVitalsMetric) {
  if (label === "web-vital") {
    event(name, {
      category: "Web Vitals",
      value: Math.round(name === "CLS" ? value * 1000 : value), // values must be integers
      label: id, // id unique to current page load
      nonInteraction: true, // avoids affecting bounce rate.
    });
  }
}

const NewLayout: React.FC = ({ children }) => <>
  <Header />
  {children}
  <Footer />
</>;

const OldLayout: React.FC = ({ children }) => {
  const { user } = useAuth();
  const router = useRouter();
  const navbar = [[], []] as const;
  const navbarItem = (uri: string, item: any) => { };

  return <>
    <nav className="navbar navbar-expand-md navbar-dark bg-red sticky-top">
      <div className="container">
        <div className="container">
          <h1 className="navbar-brand my-0 ml-lg-2 mx-3">
            <a href="/"><img alt="" src="/style/new-logo-oneline.png" /></a>
          </h1>
          <button className="navbar-toggler ml-auto" type="button" data-toggle="collapse" data-target=".navbars">
            <span className="navbar-toggler-icon"></span>
          </button>
          <div className="navbars collapse navbar-collapse ml-lg-0 ml-3">
            <div className="flex-column" style={{ flexGrow: 1 }}>
              <ul className="navbar-nav">
                {navbar[0].map(item => navbarItem(router.pathname, item))}
                <li className="w-100"></li>

                {user ? (
                  <li className="userbox nav-item nav-link">
                    <a href="/member/profil">
                      <i className="fa-solid fa-user" /> {user.uJmeno} {user.uPrijmeni}
                    </a>
                  </li>
                ) : <>
                  <li className="nav-item">
                    <Dropdown alignRight>
                      <Dropdown.Toggle variant="danger" style={{ border: 'transparent', backgroundColor: 'transparent' }}>
                        <i className="fa-solid fa-user" /> Přihlásit
                      </Dropdown.Toggle>
                      <Dropdown.Menu style={{ minWidth: '250px', padding: 0 }}>
                        <LoginForm />
                      </Dropdown.Menu>
                    </Dropdown>
                  </li>
                </>}
              </ul>
            </div>
          </div>
        </div>

        {navbar[1] && (
          <div className="w-100" id="navbar-second">
            <Container>
              <div className="navbars collapse navbar-collapse ml-lg-0 ml-3">
                <div className="flex-column" style={{ flexGrow: 1 }}>
                  <ul className="navbar-nav">
                    {navbar[1].map(item => navbarItem(router.pathname, item))}
                  </ul>
                </div>
              </div>
            </Container>
          </div>
        )}
      </div>
    </nav>

    {children}

    <div id="footer">
      <Container>
        <Row className="text-center">
          <Col className="d-flex" style={{ flexDirection: 'column', justifyContent: 'space-around' }}>
            <div>
              <a className="no-a" href="https://www.facebook.com/tkolymp" target="_blank">
                <img alt="Facebook, stránka Taneční klub Olymp" src="/style/fb-logo.png" />
              </a>
            </div>
            <div>
              design a realizace:
              <a href="mailto:jakub@zarybnicky.com">Jakub Zárybnický</a>
              <br />
              © 2018 TK Olymp
            </div>
          </Col>
          <Col>
            <span>Podporuje nás:</span>
            <div>
              <a className="no-a" href="http://www.kr-olomoucky.cz/" target="_blank">
                <img alt="Olomoucký kraj" src="/style/logo-kraj.png" />
              </a>
              <br />
              <a className="no-a" href="http://www.olomouc.eu/" target="_blank">
                <img alt="Město Olomouc" src="/style/logo-olomouc.jpg" height={85} />
              </a>
            </div>
          </Col>
        </Row>
      </Container>
    </div>
  </>;
};
