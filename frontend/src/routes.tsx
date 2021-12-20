import * as React from 'react';
import { Redirect, Switch, Route, RouteProps } from 'react-router-dom';

import { useAuth, User } from './use-auth';

import { EditorPage } from './pages/EditorPage';
import { AboutPage } from './pages/AboutPage';
import { HomePage } from './pages/HomePage';
import { LocationsPage } from './pages/LocationsPage';
import { NewsPage } from './pages/NewsPage';
import { TrainersPage } from './pages/TrainersPage';
import { GalleryPage } from './pages/GalleryPage';
import { DynamicPage } from './pages/DynamicPage';

/* import { ListGuesser, EditGuesser, ShowGuesser } from 'ra-ui-materialui'; */
const ProtectedRoute = ({ check, children, ...rest }: { check: (user: User | null) => boolean; } & RouteProps) => (
  <Route {...rest} render={() => {
    const { user } = useAuth();
    return check(user) ? children : <Redirect to="/login" />;
  }} />
);

const isLoggedIn = (user: User | null) => !!user;

export const routes = <Switch>

  <Redirect exact from="/" to="/home" />
  <Route exact path="/home"><HomePage /></Route>
  <Route exact path="/o-nas"><AboutPage /></Route>
  <Route exact path="/o-nas/kde-trenujeme"><LocationsPage /></Route>
  <Route exact path="/o-nas/treneri"><TrainersPage /></Route>

  {/* <Route exact path="/o-nas/treninkove-skupiny"><CohortsPage /></Route>
      <Route exact path="/o-nas/clenstvi"><MembershipPage /></Route>
      <Route exact path="/o-nas/galerie-mistru"><HallOfFamePage /></Route>

      <Redirect exact from="/nabizime" to="/nabizime/treninkove-programy" />
      <Route exact path="/nabizime/treninkove-programy"><TrainingOfferPage /></Route>
      <Route exact path="/nabizime/skolni-krouzky"><SchoolOfferPage /></Route>
      <Route exact path="/nabizime/vystoupeni"><ShowOfferPage /></Route> */}

  <Route exact path="/aktualne"><NewsPage /></Route>
  <Route exact path="/aktualne/:id">Show article</Route>
  {/* <Route exact path="/aktualne/:id/edit"><EditorPage /></Route> */}

  <Redirect from="/fotogalerie/:path*" to="/gallery/:path*" />
  <Redirect from="/galerie/:path*" to="/gallery/:path*" />
  <Redirect from="/gallery/:directory/foto/:id" to="/gallery/:directory/photo/:id" />
  <Route exact path="/gallery"><GalleryPage /></Route>
  <Route exact path="/gallery/:directory"><GalleryPage /></Route>
  <Route exact path="/gallery/:directory/photo/:id"><GalleryPhotoPage /></Route>

  <Redirect from="/registrace" to="/register" />
  <Redirect from="/nopassword" to="/forgotten-password" />
  <Route exact path="/login"><LoginPage /></Route>
  <Route exact path="/register"><RegisterPage /></Route>
  <Route exact path="/forgotten-password"><ForgottenPasswordPage /></Route>

  <Route exact path="/events"><EventsPage /></Route>
  <Route exact path="/events/:id"><EventsSinglePage /></Route>

  <Redirect from="/member/home" to="/dashboard" />
  <Redirect from="/member/rozpis" to="/schedule" />
  <Redirect from="/member/treninky" to="/schedule" />
  <Redirect from="/member/nabidka" to="/schedule" />
  <Redirect from="/member/clenove" to="/groups" />
  <Redirect from="/member/profil" to="/profile" />
  <ProtectedRoute exact path="/dashboard" check={isLoggedIn}><MemberDashboardPage /></ProtectedRoute>
  <ProtectedRoute exact path="/schedule" check={isLoggedIn}><MemberSchedulePage /></ProtectedRoute>
  <ProtectedRoute exact path="/documents" check={isLoggedIn}><MemberDocumentsPage /></ProtectedRoute>
  <ProtectedRoute exact path="/groups" check={isLoggedIn}><MemberGroupsPage /></ProtectedRoute>
  <ProtectedRoute exact path="/profile" check={isLoggedIn}><MemberProfilePage /></ProtectedRoute>

  {/* <Route exact path="/admin/upozorneni" render={(routeProps) =>
      <ListGuesser hasCreate resource="upozorneni"
      basePath={routeProps.match.url} {...routeProps} />} />
      <Route exact path="/admin/upozorneni/:id" render={(routeProps) =>
      <EditGuesser hasShow resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} />
      <Route exact path="/admin/upozorneni/:id/show" render={(routeProps) =>
      <ShowGuesser hasEdit resource="upozorneni"
      basePath={routeProps.match.url} id={routeProps.match.params.id} {...routeProps} />} /> */}

  <Route><DynamicPage /></Route>
</Switch>;
