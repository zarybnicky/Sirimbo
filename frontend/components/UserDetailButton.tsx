import * as React from 'react';
import { Button, DialogTitle, Dialog, ListItemIcon, List, ListItem, ListItemText } from '@mui/material';
import PersonIcon from '@mui/icons-material/Person';
import EmailIcon from '@mui/icons-material/Email';
import PhoneIcon from '@mui/icons-material/Phone';
import { UserFragment } from 'lib/graphql';

export const UserDetailButton: React.FC<{ user: UserFragment }> = ({ user }) => {
  const [open, setOpen] = React.useState(false);

  return <>
    <Button variant="text" sx={{ textTransform: 'none' }} onClick={() => setOpen(true)} startIcon={<PersonIcon />}>
      {user.uPrijmeni}, {user.uJmeno}
    </Button>

    <Dialog onClose={() => setOpen(false)} open={open}>
      <DialogTitle>{user.uJmeno} {user.uPrijmeni}</DialogTitle>
      <List sx={{ pt: 0 }}>
        <ListItem>
          <ListItemIcon><EmailIcon /></ListItemIcon>
          <ListItemText primary={user.uEmail} />
        </ListItem>
        <ListItem>
          <ListItemIcon><PhoneIcon /></ListItemIcon>
          <ListItemText primary={user.uTelefon} />
        </ListItem>
      </List>
    </Dialog>
  </>;
}
