import { Alert, Button, Grid } from '@mui/material';
import { useCreateCoupleMutation, useUserListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { AutocompleteElement } from 'react-hook-form-mui';

type FormProps = {
  man: { id: string; label: string };
  woman: { id: string; label: string };
};

export const NewCoupleForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess }) => {
  const { data: users } = useUserListQuery();
  const { mutateAsync: doCreate } = useCreateCoupleMutation({ onSuccess });

  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { control, handleSubmit } = useForm<FormProps>();
  const onSubmit = async (values: FormProps) => {
    setSubmitError(null);
    try {
      await doCreate({ man: values.man.id, woman: values.woman.id });
    } catch (e) {
      setSubmitError(e instanceof Error ? e.message : 'Něco se nepovedlo');
    }
  };

  return (
    <Grid container spacing={3} component="form" onSubmit={handleSubmit(onSubmit)}>
      <Grid item xs={12}>
        {submitError && <Alert severity="error">{submitError}</Alert>}
        <AutocompleteElement
          control={control} name="man" label="Partner" required
          options={(users?.users?.nodes || []).filter(x => x.uPohlavi === 'm').map(x => ({
            id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni} (${x.uId})`
          }))}
        />
      </Grid>
      <Grid item xs={12}>
        <AutocompleteElement
          control={control} name="woman" label="Partnerka" required
          options={(users?.users?.nodes || []).filter(x => x.uPohlavi === 'f').map(x => ({
            id: x.uId, label: `${x.uJmeno} ${x.uPrijmeni} (${x.uId})`
          }))}
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
