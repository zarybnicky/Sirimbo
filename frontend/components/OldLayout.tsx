import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { Col, Container, Dropdown, Row } from 'react-bootstrap';
import { LoginForm } from "components/LoginForm";
import { NextLinkComposed } from './Link';

export const OldLayout: React.FC = ({ children }) => {
  const { user } = useAuth();
  const router = useRouter();
  const navbar = [[], []] as const;
  const navbarItem = (uri: string, item: any) => { };

  return <>
    <nav className="navbar navbar-expand-md navbar-dark bg-red sticky-top">
      <div className="container">
        <div className="container">
          <h1 className="navbar-brand my-0 ml-lg-2 mx-3">
            <NextLinkComposed href="/">
              <img alt="" src="/style/new-logo-oneline.png" />
            </NextLinkComposed>
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
                    <NextLinkComposed href="/member/profil">
                      <i className="fa-solid fa-user" /> {user.uJmeno} {user.uPrijmeni}
                    </NextLinkComposed>
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
              <a className="no-a" href="https://www.facebook.com/tkolymp" rel="noreferrer" target="_blank">
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
              <a className="no-a" href="http://www.kr-olomoucky.cz/" rel="noreferrer" target="_blank">
                <img alt="Olomoucký kraj" src="/style/logo-kraj.png" />
              </a>
              <br />
              <a className="no-a" href="http://www.olomouc.eu/" rel="noreferrer" target="_blank">
                <img alt="Město Olomouc" src="/style/logo-olomouc.jpg" height={85} />
              </a>
            </div>
          </Col>
        </Row>
      </Container>
    </div>
  </>;
};
