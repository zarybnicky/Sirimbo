import * as React from 'react';
import { Card, CardContent, Container, Button, Checkbox as MuiCheckbox } from '@material-ui/core';
import { Alert } from '@material-ui/lab';
import { Form } from 'react-final-form'
import { Autocomplete, TextField, KeyboardDatePicker, Radios, showErrorOnBlur } from 'mui-rff';
import { useCountries } from '../data/use-countries';

import 'date-fns';
import DateFnsUtils from '@date-io/date-fns';

export const RegisterPage = () => {
  const countries = useCountries();

  /* 'skupiny' => \DBSkupiny:: get(),
   *   'countries' => \Countries:: $countries, */

  //         $poznamkyMap = [
  //             'parent' => 'Rodič tanečníka: ' . ($_POST['dancer-name'] ?? null),
  //             'dancer' => 'Tanečník/tanečnice',
  //             'other' => 'Jiný vztah: ' . ($_POST['other'] ?? null)
  //         ];
  //         $f->checkLogin($_POST['username'], 'Špatný formát přihlašovacího jména');
  //         $f->checkPassword($_POST['pass'], 'Špatný formát hesla');
  //         $f->checkEmail($_POST['email'], 'Neplatný formát emailu');
  //         $f->checkPhone($_POST['telefon'], 'Neplatný formát telefoního čísla');
  //         $f->checkDate($narozeni, 'Neplatné datum narození');
  //         $f->checkNotEmpty($_POST['rodnecislo'], 'Vyplňte rodné číslo');
  //         $f->checkNotEmpty($_POST['orientacni'], 'Vyplňte číslo orientační bydliště');
  //         $f->checkNotEmpty($_POST['city'], 'Vyplňte město bydliště');
  //         $f->checkNotEmpty($_POST['postal'], 'Vyplňte PSČ bydliště');
  //         $f->checkNotEmpty($_POST['nationality'], 'Vyplňte vaši národnost'); */

  const onSubmit = () => { };
  return <Container maxWidth="xs" style={{ margin: '4rem auto 6rem' }}>
    <Form onSubmit={onSubmit} render={(form) => <>
      {form.submitError && <Alert severity="error">{form.submitError}</Alert>}

      <Card component="form" onSubmit={form.handleSubmit}>
        <CardContent>
          {/* Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků */}
          <TextField label="Přihlašovací jméno" name="username" autoComplete="username" required />
          {/* Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků */}
          <TextField label="Heslo" name="pass" type="password" autoComplete="new-password" required />

          <TextField label="Jméno" name="jmeno" autoComplete="given-name" required />
          <TextField label="Příjmení" name="prijmeni" autoComplete="family-name" required />

          <KeyboardDatePicker label="Datum narození" name="narozeni" required dateFunsUtils={DateFnsUtils} />
          <TextField label="Rodné číslo" name="rodnecislo" required fieldProps={{
            validate: (i) => (i as string).match(/[0-9]{9,10}/) ? undefined : 'Neplatné rodné číslo',
          }} showError={showErrorOnBlur} />

          <Radios name="pohlavi" required data={[
            { label: 'Muž', value: 'm' },
            { label: 'Žena', value: 'f' }
          ]} />

          <TextField label="E-mail" name="email" type="email" required showError={showErrorOnBlur} />
          <TextField label="Telefon" name="telefon" required />

          <TextField label="Ulice" name="street" autoComplete="address-line1" required />
          <TextField label="Č. popisné" name="popisne" required />
          <TextField label="Č. orientační" name="orientacni" required />
          <TextField label="Město" name="city" autoComplete="address-level2" required />
          <TextField label="Část města" name="district" autoComplete="address-level3" required />
          <TextField label="PSČ" name="postal" autoComplete="postal-code" required />

          <Autocomplete label="Národnost" name="nationality" required getOptionValue={x => x.code} options={countries} />

        </CardContent>
        <Button
          fullWidth variant="contained" type="submit" color="primary"
          disabled={form.pristine || form.submitting}
        >Přihlásit</Button>
      </Card>
    </>} />
  </Container>
};
