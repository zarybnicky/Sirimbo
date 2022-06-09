import * as React from 'react';
import { Redirect, Switch, Route, RouteProps } from 'react-router-dom';
import { Alert } from '@material-ui/lab';

import { AppUser, useAuth } from './data/use-auth';

import { EditorPage } from './pages/EditorPage';
import { CrmPage } from './pages/CrmPage';
import { AboutPage } from './pages/AboutPage';
import { HomePage } from './pages/HomePage';
import { LocationsPage } from './pages/LocationsPage';
import { ArticlePage } from './pages/ArticlePage';
import { ArticlesPage } from './pages/ArticlesPage';
import { GalleryPage } from './pages/GalleryPage';
import { LoginPage } from './pages/LoginPage';
import { RegisterPage } from './pages/RegisterPage';
import { DashboardPage } from './pages/DashboardPage';
import { DynamicPage } from './pages/DynamicPage';
import { CohortsPage } from './pages/CohortsPage';
import { SchedulePage } from './pages/SchedulePage';
import { ProfilePage } from './pages/ProfilePage';

const ProtectedRoute = ({ check, children, ...rest }: {
  check: (user: AppUser | null) => boolean;
} & RouteProps) => {
  const auth = useAuth();
  return <Route {...rest} render={({ location }) => {
    if (auth.isLoading) {
      return null;          // TODO
    };
    if (!check(auth.user)) {
      if (auth.user) {
        return <Alert severity="error">Nedostatečná oprávnění</Alert>;
      } else {
        return <Redirect to={{ pathname: '/login', state: { from: location } }} />
      }
    }
    return children;
  }} />;
};

const ExternalRedirect = ({ to, ...routeProps }: { to: string; } & Omit<RouteProps, 'children' | 'render'>) => {
  return <Route {...routeProps} render={() => {
    window.location.assign(to);
    return null;
  }} />;
};

const ForgottenPasswordPage = () => <React.Fragment>Forgotten Password</React.Fragment>;
const EventsPage = () => <React.Fragment>Events</React.Fragment>;
const EventPage = () => <React.Fragment>Event</React.Fragment>;
const DocumentsPage = () => <React.Fragment>Documents</React.Fragment>;
const GroupOverviewPage = () => <React.Fragment>GroupOverview</React.Fragment>;

const isLoggedIn = (user: AppUser | null) => !!user;
const isAdmin = (user: AppUser | null) => !!user && (user?.permissionByUGroup?.peUsers || 1) > 1;

export const routes = <Switch>

  <Redirect exact from="/" to="/home" />
  <Route exact path="/home"><HomePage /></Route>
  <Redirect from="/oklubu/:path*" to="/o-nas/:path*" />
  <Route exact path="/o-nas"><AboutPage /></Route>
  <Route exact path="/o-nas/kde-trenujeme"><LocationsPage /></Route>
  <Route exact path="/o-nas/treninkove-skupiny"><CohortsPage /></Route>

  {/* <Route exact path="/o-nas/clenstvi"><MembershipPage /></Route>
      <Route exact path="/o-nas/galerie-mistru"><HallOfFamePage /></Route>

      <Redirect exact from="/nabizime" to="/nabizime/treninkove-programy" />
      <Route exact path="/nabizime/treninkove-programy"><TrainingOfferPage /></Route>
      <Route exact path="/nabizime/skolni-krouzky"><SchoolOfferPage /></Route>
      <Route exact path="/nabizime/vystoupeni"><ShowOfferPage /></Route> */}

  <Redirect from="/aktualne/:path*" to="/articles/:path*" />
  <Redirect from="/fotogalerie/:path*" to="/gallery/:path*" />
  <Redirect from="/galerie/:path*" to="/gallery/:path*" />
  <Redirect from="/gallery/:directory/foto/:id" to="/gallery/:directory/photo/:id" />
  <Redirect from="/registrace" to="/register" />
  <Redirect from="/nopassword" to="/forgotten-password" />
  <Redirect from="/member/home" to="/dashboard" />
  <Redirect from="/member/rozpis" to="/schedule" />
  <Redirect from="/member/treninky" to="/schedule" />
  <Redirect from="/member/nabidka" to="/schedule" />
  <Redirect from="/member/clenove" to="/groups" />
  <Redirect from="/member/profil" to="/profile" />

  <Route exact path="/articles"><ArticlesPage /></Route>
  <Route exact path="/articles/:id"><ArticlePage /></Route>

  <Route exact path="/gallery"><GalleryPage /></Route>
  <Route exact path="/gallery/:dir"><GalleryPage /></Route>
  <Route exact path="/gallery/:dir/photo/:photo"><GalleryPage /></Route>

  <Route exact path="/login"><LoginPage /></Route>
  <Route exact path="/register"><RegisterPage /></Route>
  <Route exact path="/forgotten-password"><ForgottenPasswordPage /></Route>

  <Redirect exact from="/events" to="/events/public" />
  <Route exact path="/events/public"><EventsPage /></Route>
  <Route exact path="/events/private"><EventsPage /></Route>
  <Route exact path="/events/:id"><EventPage /></Route>

  <ProtectedRoute exact path="/dashboard" check={isLoggedIn}><DashboardPage /></ProtectedRoute>
  <ProtectedRoute exact path="/schedule" check={isLoggedIn}><SchedulePage /></ProtectedRoute>
  <ProtectedRoute exact path="/documents" check={isLoggedIn}><DocumentsPage /></ProtectedRoute>
  <ProtectedRoute exact path="/groups" check={isLoggedIn}><GroupOverviewPage /></ProtectedRoute>
  <ProtectedRoute exact path="/profile" check={isLoggedIn}><ProfilePage /></ProtectedRoute>

  <ProtectedRoute exact path="/editor" check={isAdmin}><EditorPage /></ProtectedRoute>
  <ProtectedRoute exact path="/crm" check={isAdmin}><CrmPage /></ProtectedRoute>
  <ExternalRedirect path="/admin" to="/admin/rozpis" />

  <Route><DynamicPage /></Route>
</Switch>;
