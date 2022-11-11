import { Button, Grid, Typography } from '@mui/material';
import { UserFragment, UserInput, useCreateUserMutation, useUpdateUserMutation, useRoleListQuery, useCohortListQuery } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { SelectElement, CheckboxElement, DatePickerElement, RadioButtonGroup, TextFieldElement } from 'react-hook-form-mui';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { useCountries } from 'lib/data/use-countries';

type FormProps = Pick<UserInput, 'uLogin' | 'uJmeno' | 'uPrijmeni' | 'uNarozeni'
  | 'uRodneCislo' | 'uPohlavi' | 'uEmail' | 'uTelefon' | 'uStreet' |
  'uConscriptionNumber' | 'uOrientationNumber' | 'uCity' |
  'uDistrict' | 'uPostalCode' | 'uNationality' | 'uPoznamky' |
  'uDancer' | 'uTeacher' | 'uBan' | 'uLock' | 'uSystem' |
  'uGroup' | 'uSkupina' | 'uPass'>;

export const UserForm: React.FC<{
  data?: UserFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateUserMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateUserMutation({ onSuccess });

  const countries = useCountries();
  const { data: cohorts } = useCohortListQuery();
  const { data: roles } = useRoleListQuery();

  const { control, handleSubmit } = useForm<FormProps>({
    defaultValues: {
      uJmeno: data?.uJmeno,
      uPrijmeni: data?.uPrijmeni,
      uNarozeni: data?.uNarozeni,
      uRodneCislo: data?.uRodneCislo,
      uPohlavi: data?.uPohlavi,
      uEmail: data?.uEmail,
      uTelefon: data?.uTelefon,
      uStreet: data?.uStreet,
      uConscriptionNumber: data?.uConscriptionNumber,
      uOrientationNumber: data?.uOrientationNumber,
      uCity: data?.uCity,
      uDistrict: data?.uDistrict,
      uPostalCode: data?.uPostalCode,
      uNationality: data?.uNationality,
      uPoznamky: data?.uPoznamky,
      uDancer: data?.uDancer,
      uTeacher: data?.uTeacher,
      uBan: data?.uBan,
      uLock: data?.uLock,
      uSystem: data?.uSystem,
      uGroup: data?.uGroup,
      uSkupina: data?.uSkupina,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.uId, patch: values });
    } else {
      await doCreate({
        input: {
          ...values,
          uLogin: values.uLogin.toLowerCase(),
          uLock: false,
        }
      });
    }
  });

  return (
    <Grid container spacing={1.5} component="form" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox grid error={onSubmit.error} />
      {!data && <>
        <Grid item xs={12} md={6}>
          <TextFieldElement fullWidth control={control} name="uLogin"
            label="Uživatelské jméno" required />
        </Grid>
        <Grid item xs={12} md={6}>
          <TextFieldElement fullWidth control={control} type="password" name="uPass" label="Heslo" required />
        </Grid>
      </>}

      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uJmeno" label="Jméno" required />
      </Grid>
      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uPrijmeni" label="Příjmení" required />
      </Grid>

      <Grid item xs={12} md={6}>
        <DatePickerElement inputProps={{ fullWidth: true }} control={control}
          label="Datum narození" name="uNarozeni" required />
      </Grid>
      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uRodneCislo"
          label="Rodné číslo" required placeholder="1111119999"
          validation={{
            pattern: {
              value: /[0-9]{9,10}/,
              message: 'Neplatné rodné číslo',
            },
          }}
        />
      </Grid>

      <RadioButtonGroup
        control={control} row name="uPohlavi" required
        options={[{ id: 'm', label: 'Muž' }, { id: 'f', label: 'Źena' }]}
      />

      <Grid item xs={12} className="mt-4">
        <Typography variant="caption">Kontaktní údaje</Typography>
      </Grid>

      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} type="email" name="uEmail" label="E-mail" required />
      </Grid>
      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} type="tel" name="uTelefon" label="Telefon" required />
      </Grid>

      <Grid item xs={12} className="mt-4">
        <Typography variant="caption">Bydliště</Typography>
      </Grid>

      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uStreet" label="Ulice" required />
      </Grid>
      <Grid item xs={12} md={3}>
        <TextFieldElement fullWidth control={control} name="uConscriptionNumber" label="Č.popisné" />
      </Grid>
      <Grid item xs={12} md={3}>
        <TextFieldElement fullWidth control={control} name="uOrientationNumber" label="Č.orientační" />
      </Grid>

      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uCity" label="Ulice" required />
      </Grid>
      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uDistrict" label="Č.popisné" />
      </Grid>
      <Grid item xs={12} md={6}>
        <TextFieldElement fullWidth control={control} name="uPostalCode" label="Č.orientační" />
      </Grid>

      <Grid item xs={12}>
        <SelectElement control={control} fullWidth
          label="Národnost" name="uNationality" required
          options={countries.map(x => ({ id: x.code, label: x.label }))}
        />
      </Grid>

      <Grid item xs={12} style={{ marginTop: '1rem' }}>
        <Typography variant="overline">Tréninkové údaje</Typography>
      </Grid>

      <Grid item xs={12}>
        <SelectElement control={control} fullWidth
          label="Tréninková skupina" name="uSkupina"
          options={cohorts?.skupinies?.nodes?.map(x => ({ id: x.sId, label: x.sName })) || []}
        />
      </Grid>

      <Grid item xs={12}>
        <TextFieldElement fullWidth control={control} name="uPoznamky" label="Poznámka" rows={3} multiline required />
      </Grid>

      <Grid item xs={12}>
        <CheckboxElement control={control} name="uDancer" value="1" label="Aktivní tanečník" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="uTeacher" value="1" label="Trenér" />
      </Grid>

      <Grid item xs={12} className="mt-4">
        <Typography variant="overline">Přístupy</Typography>
      </Grid>

      <Grid item xs={12}>
        <SelectElement control={control} fullWidth
          label="Uživatelská role" name="uGroup"
          options={roles?.permissions?.nodes?.map(x => ({ id: x.peId, label: x.peName })) || []}
        />
      </Grid>

      <Grid item xs={12}>
        <CheckboxElement control={control} name="uBan" value="1"
          label="Neaktivní uživatel (ban)" />
      </Grid>
      <Grid item xs={12}>
        <CheckboxElement control={control} name="uSystem" value="1" label="Systémový uživatel" />
      </Grid>

      <Grid item xs={12}>
        <Button fullWidth variant="contained" type="submit" color="primary" disabled={onSubmit.loading}>Uložit</Button>
      </Grid>
    </Grid >
  );
};
