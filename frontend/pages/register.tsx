import * as React from 'react';
import { Alert, Card, Grid, CardContent, Container, Button, CardActions, Typography } from '@mui/material';
import { AutocompleteElement, TextFieldElement, DatePickerElement, RadioButtonGroup } from 'react-hook-form-mui';
import { Heading } from '../components/Heading';
import { useCountries } from 'lib/data/use-countries';
import { useCohorts } from 'lib/data/use-cohorts';

import DateFnsUtils from 'date-fns';
import { useForm } from 'react-hook-form';

export const RegisterPage = () => {
  const countries = useCountries();
  const cohorts = useCohorts();
  const [submitError, setSubmitError] = React.useState<string | null>(null);
  const { control, handleSubmit, watch, formState: { isDirty, isValid, isSubmitting } } = useForm();

  //         $poznamkyMap = [
  //             'parent' => 'Rodič tanečníka: ' . ($_POST['dancer-name'] ?? null),
  //             'dancer' => 'Tanečník/tanečnice',
  //             'other' => 'Jiný vztah: ' . ($_POST['other'] ?? null)
  //         ];

  const onSubmit = async (values: object) => {
    setSubmitError(null);
    try {
      // register
    } catch (e) {
      if (e instanceof Error) {
        setSubmitError(e.message);
      } else {
        setSubmitError('Něco se nepovedlo, zkuste to prosím znovu');
      }
    }
  };

  return (
    <Container maxWidth="md" style={{ margin: '1rem auto 1rem' }}>
      <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Registrace" image="" />

      <Card component="form" onSubmit={handleSubmit(onSubmit)}>
        <CardContent>
          <Grid container spacing={1}>
            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Přihlašovací údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              {/* Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků */}
              <TextFieldElement control={control} label="Přihlašovací jméno" name="username" autoComplete="username" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              {/* Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků */}
              <TextFieldElement control={control} label="Heslo" name="pass" type="password" autoComplete="new-password" required />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Osobní údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Jméno" name="jmeno" autoComplete="given-name" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Příjmení" name="prijmeni" autoComplete="family-name" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <DatePickerElement control={control} label="Datum narození" name="narozeni" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Rodné číslo" name="rodnecislo" required validation={{
                pattern: {
                  value: /[0-9]{9,10}/,
                  message: 'Neplatné rodné číslo',
                },
              }} />
            </Grid>

            <Grid item xs={12}>
              <RadioButtonGroup control={control} name="pohlavi" required options={[
                { label: 'Muž', id: 'm' },
                { label: 'Žena', id: 'f' }
              ]} />
            </Grid>

            <Grid item xs={12}>
              <AutocompleteElement control={control}
                label="Národnost" name="nationality" required
                options={countries.map(x => ({ id: x.code, label: x.label }))}
              />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Kontaktní údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="E-mail" name="email" type="email" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Telefon" name="telefon" required />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Adresa</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Ulice" name="street" autoComplete="address-line1" />
            </Grid>
            <Grid item xs={6} sm={3}>
              <TextFieldElement control={control} type="number" label="Č. popisné" name="popisne" />
            </Grid>
            <Grid item xs={6} sm={3}>
              <TextFieldElement control={control} type="number" label="Č. orientační" name="orientacni" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Město" name="city" autoComplete="address-level2" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="Část města" name="district" autoComplete="address-level3" />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement control={control} label="PSČ" name="postal" autoComplete="postal-code" required />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Tréninkové údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <AutocompleteElement control={control}
                label="Tréninková skupina" name="skupina" required
                options={cohorts.map(x => ({ id: x.sId, label: x.sName }))}
              />
            </Grid>

            <Grid item xs={12} sm={6}>
              <AutocompleteElement control={control}
                label="Vztah ke klubu" name="poznamky" required
                options={[
                  { id: 'dancer', label: 'Tanečník/tanečnice' },
                  { id: 'parent', label: 'Rodič tanečníka' },
                  { id: 'other', label: 'Jiný' },
                ]}
              />
              {watch('poznamky') === 'parent' && (
                <TextFieldElement control={control} label="Jméno tanečníka" name="dancer-name" />
              )}
              {watch('poznamky') === 'other' && (
                <TextFieldElement control={control} label="Popište svůj vztah ke klubu" name="other" placeholder="vztah ke klubu" />
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
          {submitError && <Alert severity="error">{submitError}</Alert>}
          <Button
            fullWidth variant="contained" type="submit" color="primary"
            disabled={isValid && !isSubmitting && isDirty}
          >Přihlásit</Button>
        </CardActions>
      </Card>
    </Container>
  );
};

export default RegisterPage;
