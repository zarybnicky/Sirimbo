import { useAuth } from 'lib/data/use-auth';
import { useRouter } from 'next/router';
import { Col, Container, Dropdown, Navbar, Row } from 'react-bootstrap';
import { LoginForm } from "components/LoginForm";
import { NextLinkComposed } from './Link';
import { FormControlLabel, Switch } from '@mui/material';
import { useConfig } from 'lib/use-config';
import { usePermissions, PermissionKey, PermissionLevel } from 'lib/data/use-permissions';

type NavbarItem
  = string
  | [string, string]
  | [string, string, NavbarItem[]]
  | [string, string, NavbarItem[], [PermissionKey, PermissionLevel]];

const topMenu: NavbarItem[] = [
  ['Klub', '/', [
    ['Kluboví trenéři', '/oklubu/klubovi-treneri'],
    ['Externí trenéři', '/oklubu/externi-treneri'],
    ['Kde trénujeme', '/oklubu/saly'],
  ]],
  ['Aktuality', '/aktualne'],
  ['Videa', '/video'],
  ['Fotogalerie', '/fotogalerie'],
  ['Kontakt', '/kontakt'],
];

// if logged in
const bottomMenu: NavbarItem[] = [
  ['Nástěnka', '/dashboard'],
  ['Tréninky', '/member/treninky'],
  ['Akce', '/member/akce'],
  ['Dokumenty', '/member/dokumenty'],
  ['Členové', '/member/clenove'],
  ['Profil', '/member/profil'],
  'w-100',
  ['Administrace', '/admin', [
    ['Uživatelé', '/admin/users', [], [PermissionKey.peUsers, PermissionLevel.P_OWNED]],
    ['Skupiny', '/admin/skupiny', [], [PermissionKey.peSkupiny, PermissionLevel.P_OWNED]],
    ['Platby', '/admin/platby', [], [PermissionKey.pePlatby, PermissionLevel.P_OWNED]],
    ['Páry', '/admin/pary', [], [PermissionKey.pePary, PermissionLevel.P_OWNED]],
    ['Články', '/admin/aktuality', [], [PermissionKey.peAktuality, PermissionLevel.P_OWNED]],
    ['Nástěnka', '/admin/nastenka', [], [PermissionKey.peNastenka, PermissionLevel.P_OWNED]],
    ['Rozpis', '/admin/rozpis', [], [PermissionKey.peRozpis, PermissionLevel.P_OWNED]],
    ['Nabídka', '/admin/nabidka', [], [PermissionKey.peNabidka, PermissionLevel.P_OWNED]],
    ['Akce', '/admin/akce', [], [PermissionKey.peAkce, PermissionLevel.P_OWNED]],
    ['Galerie', '/admin/galerie', [], [PermissionKey.peGalerie, PermissionLevel.P_OWNED]],
    ['Video', '/admin/video', [], [PermissionKey.peAktuality, PermissionLevel.P_OWNED]],
    ['Dokumenty', '/admin/dokumenty', [], [PermissionKey.peDokumenty, PermissionLevel.P_OWNED]],
    ['Oprávnění', '/admin/permissions', [], [PermissionKey.pePermissions, PermissionLevel.P_OWNED]],
  ], [PermissionKey.peNastenka, PermissionLevel.P_OWNED]]
];

const NavbarItem: React.FC<{ pathname: string; item: NavbarItem; }> = ({ item, pathname }) => {
  const permissions = usePermissions();

  if (typeof item === 'string') {
    return <div className={item} />;
  }
  if (item[3] && permissions[item[3][0]]! < item[3][1]) {
    return null;
  }
  const active = (item[1] === pathname || pathname.includes(item[1])) ? ' active' : '';

  if (!item[2]) {
    return <li className={`nav-item ${active}`}>
      <NextLinkComposed className="nav-link" href={item[1]}>
        {item[0]}
      </NextLinkComposed>
    </li>;
  }

  return (
    <Dropdown className="nav-item">
      <Dropdown.Toggle className="nav-link" style={{ border: 'transparent', backgroundColor: 'transparent' }}>
        {item[0]}
      </Dropdown.Toggle>
      <Dropdown.Menu style={{ minWidth: '250px' }}>
        {item[2].map((sub: NavbarItem, i) => {
          if (sub[3] && permissions[sub[3][0] as PermissionKey]! < sub[3][1]) {
            return null;
          }
          return <a key={i} className="dropdown-item" href={sub[1]}>{sub[0]}</a>;
        })}
      </Dropdown.Menu>
    </Dropdown>
  );
};

export const OldLayout: React.FC = ({ children }) => {
  const { user } = useAuth();
  const { layout, setLayout } = useConfig();
  const { pathname } = useRouter();

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
                {topMenu.map((item, i) => (
                  <NavbarItem key={i} item={item} pathname={pathname} />
                ))}

                <li className="w-100"></li>

                {user ? (
                  <li className="userbox nav-item nav-link">
                    <NextLinkComposed href="/member/profil">
                      <i className="fa-solid fa-user" /> {user.uJmeno} {user.uPrijmeni}
                    </NextLinkComposed>
                  </li>
                ) : <>
                  <li className="nav-item">
                    <NextLinkComposed className="nav-link active" href="/login">
                      <i className="fa-solid fa-user" /> Přihlásit
                    </NextLinkComposed>
                  </li>
                </>}
              </ul>
            </div>
          </div>
        </div>

        {user && (
          <div className="w-100" id="navbar-second">
            <Container>
              <Navbar.Collapse className="navbars ml-lg-0 ml-3">
                <div className="flex-column" style={{ flexGrow: 1 }}>
                  <ul className="navbar-nav">
                    {bottomMenu.map((item, i) => (
                      <NavbarItem key={i} item={item} pathname={pathname} />
                    ))}
                  </ul>
                </div>
              </Navbar.Collapse>
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
              © 2022 Taneční klub Olymp Olomouc, z. s.
            </div>
            <div>
              <FormControlLabel
                control={
                  <Switch
                    checked={layout === 'new'}
                    onChange={(_, isNew) => setLayout(isNew ? 'new' : 'old')}
                  />
                }
                label="Nový design"
              />
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
