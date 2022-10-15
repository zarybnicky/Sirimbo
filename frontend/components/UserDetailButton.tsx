import * as React from 'react';
import { Button, DialogTitle, Dialog, ListItemIcon, List, ListItem, ListItemText } from '@mui/material';
import PersonIcon from '@mui/icons-material/Person';
import EmailIcon from '@mui/icons-material/Email';
import PhoneIcon from '@mui/icons-material/Phone';
import { UserDetailFragment } from 'lib/graphql';

export const UserDetailButton: React.FC<{ user: UserDetailFragment }> = ({ user }) => {
  const [open, setOpen] = React.useState(false);

  return <>
    <Button variant="outlined" onClick={() => setOpen(true)} startIcon={<PersonIcon />}>
      {user.uJmeno} {user.uPrijmeni}
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
