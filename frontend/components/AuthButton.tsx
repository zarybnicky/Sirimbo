import * as React from 'react';
import { useRouter } from 'next/router';
import { Button, Menu, MenuItem } from '@mui/material';
import { PopupState as PopupStateType } from 'material-ui-popup-state/core';
import PopupState, { bindTrigger, bindMenu } from 'material-ui-popup-state';
import { useAuth } from 'lib/data/use-auth';
import AccountCircle from '@mui/icons-material/AccountCircle';
import { NextLinkComposed } from './Link';

export const AuthButton = ({ }) => {
  const auth = useAuth();
  const router = useRouter();

  const signOut = (popupState: PopupStateType) => {
    popupState.close();
    auth.signOut();
    router.push('/');
  };

  if (!auth.user) {
    return <div>
      <Button
        component={NextLinkComposed} href="/login" color="inherit"
        startIcon={<AccountCircle />}
        style={{ textDecoration: 'underline' }}
      >
        Přihlásit
      </Button>
    </div>;
  }
  return <PopupState variant="popover" popupId="demoMenu">
    {(popupState) => <>
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
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
      >
        <MenuItem component={NextLinkComposed} href="/member/profil">Můj profil</MenuItem>
        <MenuItem onClick={() => signOut(popupState)}>Odhlásit se</MenuItem>
      </Menu>
    </>}
  </PopupState>;
};
