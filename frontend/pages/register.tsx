import * as React from 'react';
import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { TextFieldElement } from 'components/TextField';
import { useCountries } from 'lib/data/use-countries';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from 'components/ErrorBox';
import { Card } from 'components/Card';
import { useRouter } from 'next/router';
import { SubmitButton } from 'components/SubmitButton';
import { RadioButtonGroupElement } from 'components/RadioButtomGroupElement';
import { toast } from 'react-toastify';
import { CohortListDocument } from 'lib/graphql/Cohorts';
import { RegisterDocument } from 'lib/graphql/CurrentUser';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlMutation, useGqlQuery } from 'lib/query';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const countries = useCountries();
  const { data: cohorts } = useGqlQuery(CohortListDocument, { visible: true });
  const { mutateAsync: register } = useGqlMutation(RegisterDocument);
  const { control, handleSubmit, watch } = useForm();

  const onSubmit = useAsyncCallback(async (values: any) => {
    await register({
      input: {
        ...values,
        username: values.username.toLowerCase(),
        poznamky:
          values.poznamky === 'dancer'
            ? 'Tanečník/tanečnice'
            : values.poznamky === 'parent'
            ? `Rodič tanečníka: ${values.dancerName}`
            : `Jiný vztah: ${values.other}`,
        dancer: values.poznamky === 'dancer',
        nationality: values.nationality.toString(),
        skupina: values.skupina,
        narozeni: new Date(values.narozeni).toISOString().substring(0, 10),
        dancerName: undefined,
        other: undefined,
      },
    });
    toast.success(
      'Registrace úspěšně proběhla. Během několika dnů vám na email příjde potvrzení vašeho účtu, které vyřizuje administrátor ručně',
    );
    router.push('/');
  });

  return (
    <Item>
      <Card>
        <form
          className="grid md:grid-cols-2 gap-2"
          onSubmit={handleSubmit(onSubmit.execute)}
        >
          <h4 className="text-xl font-bold mb-2 col-full">Registrace</h4>

          <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
            Přihlašovací údaje
          </div>

          <TextFieldElement
            control={control}
            label="Přihlašovací jméno"
            name="username"
            autoComplete="username"
            required
            helperText="Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků"
          />
          <TextFieldElement
            control={control}
            label="Heslo"
            name="password"
            type="password"
            autoComplete="new-password"
            required
            helperText="Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků"
          />

          <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
            Osobní údaje
          </div>

          <TextFieldElement
            control={control}
            label="Jméno"
            name="jmeno"
            autoComplete="given-name"
            required
          />
          <TextFieldElement
            control={control}
            label="Příjmení"
            name="prijmeni"
            autoComplete="family-name"
            required
          />
          <TextFieldElement
            control={control}
            type="date"
            label="Datum narození"
            name="narozeni"
            required
          />
          <TextFieldElement
            control={control}
            label="Rodné číslo"
            name="rodneCislo"
            required
            validation={{
              pattern: {
                value: /[0-9]{9,10}/,
                message: 'Neplatné rodné číslo',
              },
            }}
          />

          <div className="col-full grid gap-2">
            <SelectElement
              control={control}
              label="Národnost"
              name="nationality"
              required
              options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
            />

            <RadioButtonGroupElement
              control={control}
              name="pohlavi"
              required
              options={[
                { label: 'Muž', id: 'm' },
                { label: 'Žena', id: 'f' },
              ]}
            />
          </div>

          <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
            Kontaktní údaje
          </div>

          <TextFieldElement
            control={control}
            label="E-mail"
            name="email"
            type="email"
            required
          />
          <TextFieldElement control={control} label="Telefon" name="telefon" required />

          <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
            Adresa
          </div>

          <TextFieldElement
            control={control}
            label="Ulice"
            name="street"
            autoComplete="address-line1"
            required
          />
          <div className="grid grid-cols-2 gap-2">
            <TextFieldElement
              control={control}
              type="number"
              label="Č. popisné"
              name="popisne"
            />
            <TextFieldElement
              control={control}
              type="number"
              label="Č. orientační"
              name="orientacni"
              required
            />
          </div>
          <TextFieldElement
            control={control}
            label="Město"
            name="city"
            autoComplete="address-level2"
            required
          />
          <TextFieldElement
            control={control}
            label="Část města"
            name="district"
            autoComplete="address-level3"
          />
          <TextFieldElement
            control={control}
            label="PSČ"
            name="postal"
            autoComplete="postal-code"
            required
          />

          <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
            Tréninkové údaje
          </div>

          <div className="col-full md:col-span-2">
            <SelectElement
              control={control}
              label="Tréninková skupina"
              name="skupina"
              options={
                cohorts?.skupinies?.nodes?.map((x) => ({ id: x.id, label: x.sName })) ||
                []
              }
            />
          </div>

          <div className="col-full grid gap-2 md:col-span-2">
            <RadioButtonGroupElement
              control={control}
              label="Vztah ke klubu"
              name="poznamky"
              required
              options={[
                { id: 'dancer', label: 'Tanečník/tanečnice' },
                { id: 'parent', label: 'Rodič tanečníka' },
                { id: 'other', label: 'Jiný' },
              ]}
            />
            {watch('poznamky') === 'parent' && (
              <TextFieldElement
                control={control}
                label="Jméno tanečníka"
                name="dancer-name"
              />
            )}
            {watch('poznamky') === 'other' && (
              <TextFieldElement
                control={control}
                label="Popište svůj vztah ke klubu"
                name="other"
                placeholder="vztah ke klubu"
              />
            )}
          </div>

          <div className="col-full prose mt-4">
            <p>
              Zákon č. 101/2000 Sb., o ochraně osobních údajů, ve znění pozdějších
              předpisů, ukládá
              <b>
                {' '}
                Tanečnímu klubu Olymp Olomouc, z. s., IČ: 68347286, se sídlem: Jiráskova
                381/25, Olomouc
              </b>
              (dále jen „klub“) práva a povinnosti, mezi něž mimo jiné patří i povinnost
              informovat své členy o právech, které se týkají ochrany osobních údajů
              člena, respektive budoucího člena o přístupu k osobním údajům, zpracování a
              předávání osobních údajů třetí osobě.
            </p>
            <p>
              Stvrzuji, že tuto povinnost klub splnil a souhlasím, aby klub shromažďoval a
              zpracovával mé (mého syna/dcery) osobní údaje v souladu s právy a
              povinnostmi, které mu ukládají obecně závazné právní předpisy.
            </p>
            <p>
              Dávám tímto výslovný souhlas s použitím citlivého údaje – fotografie – na
              webu klubu. Souhlasím s používáním všech fotografií a videí ze soutěží,
              tréninků, soustředění… vytvořených pro účely propagace klubu. Souhlasím s
              jejich zveřejněním na webových stránkách klubu a sociálních sítích Facebook
              a Instagram.
            </p>
            <p>
              Dojde-li ke změně v mých osobních údajích, zavazuji se je klubu oznámit
              neprodleně. Můj souhlas se zpracováním osobních údajů se bude vztahovat i na
              tyto nově oznámené osobní údaje.
            </p>
          </div>

          <ErrorBox error={onSubmit.error} />
          <SubmitButton className="w-full" loading={onSubmit.loading}>
            Registrovat
          </SubmitButton>
        </form>
      </Card>
    </Item>
  );
}

Page.staticTitle = "Registrace";
Page.requireLoggedOut = true;

export default Page;
