import * as React from 'react';
import { Card, Grid, CardContent, Container, Button, CardActions, Typography } from '@material-ui/core';
import { Alert } from '@material-ui/lab';
import { Form } from 'react-final-form'
import { Autocomplete, TextField, KeyboardDatePicker, Radios, showErrorOnBlur } from 'mui-rff';
import { Heading } from '../components/Heading';
import { useCountries } from '../data/use-countries';
import { useCohorts } from '../data/use-cohorts';

import 'date-fns';
import DateFnsUtils from '@date-io/date-fns';

export const RegisterPage = () => {
  const countries = useCountries();
  const cohorts = useCohorts();

  //         $poznamkyMap = [
  //             'parent' => 'Rodič tanečníka: ' . ($_POST['dancer-name'] ?? null),
  //             'dancer' => 'Tanečník/tanečnice',
  //             'other' => 'Jiný vztah: ' . ($_POST['other'] ?? null)
  //         ];

  const onSubmit = async (values: object) => {

  };
  return <>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Registrace" image="" />
    <Container maxWidth="md" style={{ margin: '1rem auto 1rem' }}>

      <Form onSubmit={onSubmit} render={(form) => <>
        {form.submitError && <Alert severity="error">{form.submitError}</Alert>}

        <Card component="form" onSubmit={form.handleSubmit}>
          <CardContent>
            <Grid container spacing={1}>
              <Grid item xs={12} style={{ marginTop: '1rem' }}>
                <Typography variant="caption">Přihlašovací údaje</Typography>
              </Grid>

              <Grid item xs={12} sm={6}>
                {/* Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků */}
                <TextField label="Přihlašovací jméno" name="username" autoComplete="username" required />
              </Grid>
              <Grid item xs={12} sm={6}>
                {/* Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků */}
                <TextField label="Heslo" name="pass" type="password" autoComplete="new-password" required />
              </Grid>

              <Grid item xs={12} style={{ marginTop: '1rem' }}>
                <Typography variant="caption">Osobní údaje</Typography>
              </Grid>

              <Grid item xs={12} sm={6}>
                <TextField label="Jméno" name="jmeno" autoComplete="given-name" required />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="Příjmení" name="prijmeni" autoComplete="family-name" required />
              </Grid>
              <Grid item xs={12} sm={6}>
                <KeyboardDatePicker label="Datum narození" name="narozeni" required dateFunsUtils={DateFnsUtils} />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="Rodné číslo" name="rodnecislo" required fieldProps={{
                  validate: (i) => (i as string || '').match(/[0-9]{9,10}/) ? undefined : 'Neplatné rodné číslo',
                }} showError={showErrorOnBlur} />
              </Grid>

              <Grid item xs={12}>
                <Radios name="pohlavi" required data={[
                  { label: 'Muž', value: 'm' },
                  { label: 'Žena', value: 'f' }
                ]} />
              </Grid>

              <Grid item xs={12}>
                <Autocomplete
                  label="Národnost" name="nationality" required
                  getOptionLabel={x => x.label}
                  getOptionValue={x => x.code}
                  options={countries}
                />
              </Grid>

              <Grid item xs={12} style={{ marginTop: '1rem' }}>
                <Typography variant="caption">Kontaktní údaje</Typography>
              </Grid>

              <Grid item xs={12} sm={6}>
                <TextField label="E-mail" name="email" type="email" required showError={showErrorOnBlur} />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="Telefon" name="telefon" required />
              </Grid>

              <Grid item xs={12} style={{ marginTop: '1rem' }}>
                <Typography variant="caption">Adresa</Typography>
              </Grid>

              <Grid item xs={12} sm={6}>
                <TextField label="Ulice" name="street" autoComplete="address-line1" />
              </Grid>
              <Grid item xs={6} sm={3}>
                <TextField type="number" label="Č. popisné" name="popisne" />
              </Grid>
              <Grid item xs={6} sm={3}>
                <TextField type="number" label="Č. orientační" name="orientacni" required />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="Město" name="city" autoComplete="address-level2" required />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="Část města" name="district" autoComplete="address-level3" />
              </Grid>
              <Grid item xs={12} sm={6}>
                <TextField label="PSČ" name="postal" autoComplete="postal-code" required />
              </Grid>

              <Grid item xs={12} style={{ marginTop: '1rem' }}>
                <Typography variant="caption">Tréninkové údaje</Typography>
              </Grid>

              <Grid item xs={12} sm={6}>
                <Autocomplete
                  label="Tréninková skupina" name="skupina" required
                  getOptionLabel={x => x.sName}
                  getOptionValue={x => x.sId}
                  options={cohorts}
                />
              </Grid>

              <Grid item xs={12} sm={6}>
                <Autocomplete
                  label="Vztah ke klubu" name="poznamky" required
                  getOptionLabel={x => x.label}
                  getOptionValue={x => x.id}
                  options={[
                    { id: 'dancer', label: 'Tanečník/tanečnice' },
                    { id: 'parent', label: 'Rodič tanečníka' },
                    { id: 'other', label: 'Jiný' },
                  ]}
                />
                {form.values.poznamky === 'parent' && (
                  <TextField label="Jméno tanečníka" name="dancer-name" />
                )}
                {form.values.poznamky === 'other' && (
                  <TextField label="Popište svůj vztah ke klubu" name="other" placeholder="vztah ke klubu" />
                )}
              </Grid>

              <Grid item xs={12} style={{ marginTop: '2rem' }}>
                <Typography variant="body1" gutterBottom>
                  Zákon č. 101/2000 Sb., o ochraně osobních údajů, ve znění pozdějších předpisů, ukládá
                  <b> Tanečnímu klubu Olymp Olomouc, z. s., IČ: 68347286, se sídlem: Jiráskova 381/25, Olomouc</b>
                  (dále jen „klub“) práva a povinnosti, mezi něž mimo jiné patří i povinnost informovat své členy
                  o právech, které se týkají ochrany osobních údajů člena, respektive budoucího člena o přístupu
                  k osobním údajům, zpracování a předávání osobních údajů třetí osobě.
                </Typography>
                <Typography variant="body1" gutterBottom>
                  Stvrzuji, že tuto povinnost klub splnil a souhlasím, aby klub shromažďoval a
                  zpracovával mé (mého syna/dcery) osobní údaje v souladu s právy a povinnostmi, které mu
                  ukládají obecně závazné právní předpisy.
                </Typography>
                <Typography variant="body1" gutterBottom>
                  Dávám tímto výslovný souhlas s použitím citlivého údaje – fotografie – na webu klubu.
                  Souhlasím s používáním všech fotografií a videí ze soutěží, tréninků, soustředění… vytvořených
                  pro účely propagace klubu. Souhlasím s jejich zveřejněním na webových stránkách klubu a
                  sociálních sítích Facebook a Instagram.
                </Typography>
                <Typography variant="body1">
                  Dojde-li ke změně v mých osobních údajích, zavazuji se je klubu oznámit neprodleně. Můj
                  souhlas se zpracováním osobních údajů se bude vztahovat i na tyto nově oznámené osobní
                  údaje.
                </Typography>
              </Grid>

            </Grid>
          </CardContent>
          <CardActions>
            <Button
              fullWidth variant="contained" type="submit" color="primary"
              disabled={form.pristine || form.submitting}
            >Přihlásit</Button>
          </CardActions>
        </Card>
      </>} />
    </Container>
  </>;
};
