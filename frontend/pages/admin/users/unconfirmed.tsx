import { Alert, Button, Card, CardActions, CardContent, Container, Grid, Typography } from "@mui/material";
import format from "date-fns/format";
import parseISO from "date-fns/parseISO";
import { useDeleteUserMutation, useCohortListQuery, useConfirmUserMutation, useRoleListQuery, UserPartialFragment, useUserListQuery, useUserQuery } from 'lib/graphql';
import { SelectElement, useForm } from "react-hook-form-mui";
import CheckIcon from '@mui/icons-material/Check';
import DeleteIcon from '@mui/icons-material/Delete';
import { useConfirm } from 'material-ui-confirm';
import React from "react";
import { withUserLoggedIn } from "lib/route-guards";

const UnconfirmedUser: React.FC<{
  item: UserPartialFragment;
  onProcessed: () => void;
}> = ({ item, onProcessed }) => {
  const confirm = useConfirm();
  const { data: cohorts } = useCohortListQuery({ visible: undefined });
  const { data: roles } = useRoleListQuery();
  const { control, handleSubmit } = useForm({
    defaultValues: {
      cohort: item.uSkupina,
      role: undefined as number | undefined,
    },
  });

  const { mutateAsync: confirmUser } = useConfirmUserMutation();
  const { mutateAsync: deleteUser } = useDeleteUserMutation();

  const onSubmit = React.useCallback(async (values) => {
    await confirmUser({ id: item.uId, cohort: values.cohort, role: values.role });
    onProcessed();
  }, [item, onProcessed]);

  const onDelete = React.useCallback(async () => {
    try {
      await confirm({ description: `Vymazat uživatele ${item.uLogin}?` })
      await deleteUser({ id: item.uId });
      onProcessed();
    } catch { }
  }, [item, confirm, deleteUser, onProcessed]);

  return (
    <Card sx={{ marginBottom: 2 }} component="form" onSubmit={handleSubmit(onSubmit)}>
      <CardContent>
        <Grid container spacing={2}>
          <Grid item md={6}>
            <Typography variant="h5">{item.uJmeno} {item.uPrijmeni}</Typography>
            <Typography>
              <b>Login:</b> {item.uLogin}
            </Typography>
            <Typography>
              <b>Datum narození:</b> {format(parseISO(item.uNarozeni), 'd. M. y')}
            </Typography>
            <Typography>
              <b>E-mail:</b> {item.uEmail}
            </Typography>
            <Typography>
              <b>Telefon:</b> {item.uTelefon}
            </Typography>
            <Typography>
              <b>Poznámka:</b> {item.uPoznamky}
            </Typography>
          </Grid>
          <Grid item md={6}>
            <SelectElement
              sx={{ marginTop: 1, marginBottom: 1 }}
              fullWidth required control={control} name="cohort" label="Tréninková skupina"
              options={(cohorts?.skupinies?.nodes || []).map(item => ({ id: item.sId, label: item.sName }))}
            />
            <SelectElement
              fullWidth required control={control} name="role" label="Role oprávnění"
              options={(roles?.permissions?.nodes || []).map(item => ({ id: item.peId, label: item.peName }))}
            />
          </Grid>
        </Grid>
      </CardContent>
      <CardActions sx={{ flexDirection: 'row-reverse', gap: 1 }}>
        <Button type="submit" startIcon={<CheckIcon />}>Potvrdit</Button>
        <Button onClick={onDelete} startIcon={<DeleteIcon />}>Odstranit</Button>
      </CardActions>
    </Card>
  );
};

export const UnconfirmedUsers = () => {
  const { data: users, refetch } = useUserListQuery({ confirmed: false });

  return <Container maxWidth="lg" style={{ padding: '4rem 0 6rem' }}>
    <Typography align="right" variant="h4" component="h2" gutterBottom>
      Nepotvrzení uživatelé
    </Typography>
    {users && users?.users?.nodes.length === 0 && (
      <Alert severity="info">Žádní nepotvrzení uživatelé nejsou v databázi.</Alert>
    )}
    {users?.users?.nodes?.map((item, i) => <UnconfirmedUser onProcessed={refetch} item={item} key={i} />)}
  </Container >;
};

export default withUserLoggedIn(UnconfirmedUsers);
