import * as React from 'react';
import { useHistory } from 'react-router';
import { NavLink } from 'react-router-dom';
import { Button, Menu, MenuItem, makeStyles } from '@material-ui/core';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from '../data/use-auth';

import AccountCircle from '@material-ui/icons/AccountCircle';

export const AuthButton = ({ }) => {
  const auth = useAuth();
  const history = useHistory();

  const signOut = (popupState: PopupStateType) => {
    popupState.close();
    auth.signOut();
    history.push('/');
  };

  if (!auth.user) {
    return <div>
      <Button
        component={NavLink} to="/login" color="inherit"
        startIcon={<AccountCircle />}
        style={{ textDecoration: 'underline' }}
      >
        Přihlásit
      </Button>
    </div>;
  }
  return <PopupState variant="popover" popupId="demoMenu">
    {(popupState) => <React.Fragment>
      <Button
        {...bindTrigger(popupState)}
        color="inherit"
        startIcon={<AccountCircle />}
      >
        <div style={{ display: 'flex', alignItems: 'start', lineHeight: 1.3, flexDirection: 'column' }}>
          <span style={{ textDecoration: 'underline' }}>Přihlášen</span>
          <span style={{ textTransform: 'none' }}>{auth.user?.uJmeno} {auth.user?.uPrijmeni}</span>
        </div>
      </Button>
      <Menu
        {...bindMenu(popupState)}
        getContentAnchorEl={null}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
      >
        <MenuItem onClick={() => signOut(popupState)}>Odhlásit se</MenuItem>
      </Menu>
    </React.Fragment>}
  </PopupState>;
};
