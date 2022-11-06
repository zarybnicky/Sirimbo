import * as React from 'react';
import { Card, Grid, CardContent, Container, Button, CardActions, Typography } from '@mui/material';
import { useForm, AutocompleteElement, TextFieldElement, DatePickerElement, RadioButtonGroup } from 'react-hook-form-mui';
import { useCountries } from 'lib/data/use-countries';
import format from 'date-fns/format';
import { useSnackbar } from 'notistack';
import { useCohortListQuery, useRegisterMutation } from 'lib/graphql';
import { useRequireUserLoggedOut } from 'lib/route-guards';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from 'components/ErrorBox';
import { useRouter } from 'next/router';

export default function RegisterPage() {
  useRequireUserLoggedOut()
  const router = useRouter();
  const countries = useCountries();
  const { data: cohorts } = useCohortListQuery({ visible: true });
  const { control, handleSubmit, watch, formState: { isDirty, isSubmitting } } = useForm();
  const { enqueueSnackbar } = useSnackbar();

  const { mutateAsync: register } = useRegisterMutation();

  const onSubmit = useAsyncCallback(async (values: any) => {
    await register({
      input: {
        ...values,
        username: values.username.toLowerCase(),
        poznamky: values.poznamky === 'dancer' ? 'Tanečník/tanečnice' :
          values.poznamky === 'parent' ? `Rodič tanečníka: ${values.dancerName}` :
            `Jiný vztah: ${values.other}`,
        dancer: values.poznamky === 'dancer',
        nationality: values.nationality.id.toString(),
        skupina: values.skupina.id,
        narozeni: format(new Date(values.narozeni), 'yyyy-MM-dd'),
        dancerName: undefined,
        other: undefined,
      },
    });
    enqueueSnackbar('Registrace úspěšně proběhla. Během několika dnů vám na email příjde potvrzení vašeho účtu, které vyřizuje administrátor ručně', { variant: 'success' });
    router.push('/');
  });

  return (
    <Container maxWidth="md" style={{ margin: '4rem auto 6rem' }}>
      <Card component="form" onSubmit={handleSubmit(onSubmit.execute)}>
        <CardContent>
          <Grid container spacing={1}>
            <Typography gutterBottom variant="h4" component="h2">Registrace</Typography>

            <Grid item xs={12}>
              <Typography variant="caption">Přihlašovací údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth
                control={control} label="Přihlašovací jméno" name="username" autoComplete="username" required
                helperText="Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků"
              />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth
                control={control} label="Heslo" name="password" type="password" autoComplete="new-password" required
                helperText="Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků"
              />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Osobní údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Jméno" name="jmeno" autoComplete="given-name" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Příjmení" name="prijmeni" autoComplete="family-name" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Datum narození" name="narozeni" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Rodné číslo" name="rodneCislo" required validation={{
                pattern: {
                  value: /[0-9]{9,10}/,
                  message: 'Neplatné rodné číslo',
                },
              }} />
            </Grid>

            <Grid item xs={12}>
              <AutocompleteElement control={control}
                label="Národnost" name="nationality" required
                options={countries.map(x => ({ id: x.code, label: x.label }))}
              />
            </Grid>

            <Grid item xs={12}>
              <RadioButtonGroup row control={control} name="pohlavi" required options={[
                { label: 'Muž', id: 'm' },
                { label: 'Žena', id: 'f' }
              ]} />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Kontaktní údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="E-mail" name="email" type="email" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Telefon" name="telefon" required />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="caption">Adresa</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Ulice" name="street" autoComplete="address-line1" required />
            </Grid>
            <Grid item xs={6} sm={3}>
              <TextFieldElement fullWidth control={control} type="number" label="Č. popisné" name="popisne" />
            </Grid>
            <Grid item xs={6} sm={3}>
              <TextFieldElement control={control} type="number" label="Č. orientační" name="orientacni" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Město" name="city" autoComplete="address-level2" required />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="Část města" name="district" autoComplete="address-level3" />
            </Grid>
            <Grid item xs={12} sm={6}>
              <TextFieldElement fullWidth control={control} label="PSČ" name="postal" autoComplete="postal-code" required />
            </Grid>

            <Grid item xs={12} style={{ marginTop: '1rem' }}>
              <Typography variant="overline">Tréninkové údaje</Typography>
            </Grid>

            <Grid item xs={12} sm={6}>
              <AutocompleteElement control={control}
                label="Tréninková skupina" name="skupina"
                options={cohorts?.skupinies?.nodes?.map(x => ({ id: x.sId, label: x.sName })) || []}
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
              {watch('poznamky')?.id === 'parent' && (
                <TextFieldElement fullWidth control={control} label="Jméno tanečníka" name="dancer-name" />
              )}
              {watch('poznamky')?.id === 'other' && (
                <TextFieldElement fullWidth control={control} label="Popište svůj vztah ke klubu" name="other" placeholder="vztah ke klubu" />
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

            <ErrorBox grid error={onSubmit.error} />
          </Grid>
        </CardContent>
        <CardActions>
          <Button
            fullWidth variant="contained" type="submit" color="primary"
            disabled={isSubmitting || !isDirty}
          >Registrovat</Button>
        </CardActions>
      </Card>
    </Container>
  );
};
