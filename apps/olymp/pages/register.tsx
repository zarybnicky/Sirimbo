import * as React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { TextFieldElement } from '@app/ui/fields/text';
import { useCountries } from '@app/ui/use-countries';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { Card } from '@app/ui/Card';
import { useRouter } from 'next/router';
import { SubmitButton } from '@app/ui/submit';
import { RadioButtonGroupElement } from '@app/ui/RadioButtomGroupElement';
import { toast } from 'react-toastify';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { RegisterDocument } from '@app/graphql/CurrentUser';
import type { NextPageWithLayout } from 'pages/_app';
import { useMutation, useQuery } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  username: z.string(),
  password: z.string(),
  jmeno: z.string(),
  prijmeni: z.string(),
  narozeni: z.date(),
  rodneCislo: z.string().regex(/[0-9]{9,10}/, 'Neplatné rodné číslo'),
  nationality: z.string(),
  pohlavi: z.enum(['m', 'f']),
  email: z.string().email(),
  telefon: z.string(),
  street: z.string(),
  popisne: z.string().optional(),
  orientacni: z.string().optional(),
  city: z.string(),
  district: z.string().optional(),
  postal: z.string(),
  skupina: z.string().optional(),
  poznamky: z.string().optional(),
  dancerName: z.string().optional(),
  other: z.string().optional(),
});
type FormProps = z.infer<typeof Form>;

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const countries = useCountries();
  const [{ data: cohorts }] = useQuery({query: CohortListDocument, variables: { visible: true }});
  const register = useMutation(RegisterDocument)[1];
  const { control, handleSubmit, watch } = useForm<FormProps>({ resolver: zodResolver(Form) });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await register({
      input: {
        user: {
          uLogin: values.username.toLowerCase(),
          uPoznamky:
          values.poznamky === 'dancer'
            ? 'Tanečník/tanečnice'
            : values.poznamky === 'parent'
            ? `Rodič tanečníka: ${values.dancerName || ''}`
            : `Jiný vztah: ${values.other || ''}`,
          uDancer: values.poznamky === 'dancer',
          uNationality: values.nationality.toString(),
          uSkupina: values.skupina,
          uNarozeni: new Date(values.narozeni).toISOString().substring(0, 10),
          uCity: values.city,
          uEmail: values.email,
          uJmeno: values.jmeno,
          uPass: values.password,
          uPohlavi: values.pohlavi,
          uPostalCode: values.postal,
          uPrijmeni: values.prijmeni,
          uStreet: values.street,
          uTelefon: values.telefon,
          uConscriptionNumber: values.popisne,
          uDistrict: values.district,
          uOrientationNumber: values.orientacni,
          uRodneCislo: values.rodneCislo,
        },
      },
    });
    toast.success(
      'Registrace úspěšně proběhla. Během několika dnů vám na email příjde potvrzení vašeho účtu, které vyřizuje administrátor ručně',
    );
    await router.push('/');
  });

  return (
    <div className="container">
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
          />

          <div className="col-full grid gap-2">
            <ComboboxElement
              control={control}
              label="Národnost"
              name="nationality"
              options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
            />

            <RadioButtonGroupElement
              control={control}
              name="pohlavi"
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
            <ComboboxElement
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
                name="dancerName"
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

          <div className="col-full prose prose-accent mt-4">
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

          <FormError error={onSubmit.error} />
          <SubmitButton className="w-full" loading={onSubmit.loading}>
            Registrovat
          </SubmitButton>
        </form>
      </Card>
    </div>
  );
}

Page.staticTitle = "Registrace";
Page.requireLoggedOut = true;

export default Page;
