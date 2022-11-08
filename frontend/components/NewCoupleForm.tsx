import { Button, Grid } from '@mui/material';
import { useCreateCoupleMutation, useUserListQuery } from 'lib/graphql';
import React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'react-hook-form-mui';
import { ErrorBox } from './ErrorBox';

type FormProps = {
  man: { id: string; label: string };
  woman: { id: string; label: string };
};

export const NewCoupleForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const { data: users } = useUserListQuery();
  const men = React.useMemo(() => (users?.users?.nodes || [])
    .filter(x => x.uPohlavi === 'm')
    .map(x => ({ id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni} (${x.uId})` })), [users]);
  const women = React.useMemo(() => (users?.users?.nodes || [])
    .filter(x => x.uPohlavi === 'f')
    .map(x => ({ id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni} (${x.uId})` })), [users])

  const { mutateAsync: doCreate } = useCreateCoupleMutation({ onSuccess });

  const { control, handleSubmit } = useForm<FormProps>();
  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doCreate({ man: values.man.id, woman: values.woman.id });
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      <Grid item xs={12}>
        <SelectElement
          control={control} name="man" label="Partner" required
          options={men}
        />
      </Grid>
      <Grid item xs={12}>
        <SelectElement
          control={control} name="woman" label="Partnerka" required
          options={women}
        />
      </Grid>
      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary">
          Spárovat
        </Button>
      </Grid>
    </Grid>
  );
};
