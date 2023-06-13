import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { RadioButtonGroupElement } from 'components/RadioButtomGroupElement';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { useCountries } from 'lib/data/use-countries';
import { SubmitButton } from './SubmitButton';
import { RoleListDocument } from '@app/graphql/Roles';
import { CohortListDocument } from '@app/graphql/Cohorts';
import {CreateUserDocument, DeleteUserDocument, UpdateUserDocument, UserDocument} from '@app/graphql/User';
import { useMutation, useQuery } from 'urql';
import { useRouter } from 'next/router';
import { toast } from 'react-toastify';
import { ErrorPage } from './ErrorPage';
import { DeleteButton } from './DeleteButton';
import { User } from 'lib/entities';
import { RichTextEditor } from './RichTextEditor';
import { TitleBar } from './layout/TitleBar';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';

const Form = z.object({
  uLogin: z.string(),
  uPass: z.string(),
  uJmeno: z.string(),
  uPrijmeni: z.string(),
  uNarozeni: z.string(),
  uRodneCislo: z.string().regex(/[0-9]{9,10}/, 'Neplatné rodné číslo').optional(),
  uPohlavi: z.enum(['m', 'f']),
  uEmail: z.string().email(),
  uTelefon: z.string(),
  uStreet: z.string(),
  uConscriptionNumber: z.string().optional(),
  uOrientationNumber: z.string().optional(),
  uCity: z.string(),
  uDistrict: z.string().optional(),
  uPostalCode: z.string(),
  uNationality: z.string(),
  uPoznamky: z.string().optional(),
  uDancer: z.boolean().default(false),
  uTeacher: z.boolean().default(false),
  uBan: z.boolean().default(false),
  uLock: z.boolean().default(false),
  uSystem: z.boolean().default(false),
  uGroup: z.string(),
  uSkupina: z.string(),
});
type FormProps = z.infer<typeof Form>;

const entity = User;

export const UserForm = ({ id = '' }: { id?: string; }) => {
  const router = useRouter();
  const [query] = useQuery({ query: UserDocument, variables: { id } });
  const data = query.data?.user;
  const title = id ? `${data?.uJmeno??''} ${data?.uPrijmeni ??''}}` : 'Nový uživatel';

  const create = useMutation(CreateUserDocument)[1];
  const update = useMutation(UpdateUserDocument)[1];

  const countries = useCountries();
  const [{ data: roles }] = useQuery({query: RoleListDocument});
  const [{ data: cohorts }] = useQuery({query: CohortListDocument});

  const { reset, control, handleSubmit } = useForm<FormProps>({ resolver: zodResolver(Form) });
  React.useEffect(() => {
    reset(Form.optional().parse(data));
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const { uLogin: _uLogin, uPass, ...patch } = values;
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({
        input: {
          ...patch,
          uPass,
          uLogin: _uLogin.toLowerCase(),
          uLock: false,
        },
      });
      const id = res.data?.createUser?.user?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(entity.editRoute(id));
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.user === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={entity.listRoute} title={title} >
        {id && (
          <DeleteButton
            doc={DeleteUserDocument}
            id={id}
            title="smazat uživatele"
            onDelete={() => router.push(entity.listRoute)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <ErrorBox error={onSubmit.error} />
      {id ? (
        <TextFieldElement
          control={control}
          name="uLogin"
          label="Uživatelské jméno"
          disabled
        />
      ) : (
        <>
          <TextFieldElement
            control={control}
            name="uLogin"
            label="Uživatelské jméno"
            required
          />
          <TextFieldElement
            control={control}
            type="password"
            name="uPass"
            label="Heslo"
            required
          />
        </>
      )}

      <TextFieldElement control={control} name="uJmeno" label="Jméno" required />
      <TextFieldElement control={control} name="uPrijmeni" label="Příjmení" required />

      <TextFieldElement
        type="date"
        control={control}
        label="Datum narození"
        name="uNarozeni"
        required
      />
      <TextFieldElement
        control={control}
        name="uRodneCislo"
        label="Rodné číslo"
        required
        placeholder="1111119999"
      />

      <div>
        <RadioButtonGroupElement
          control={control}
          name="uPohlavi"
          options={[
            { id: 'm', label: 'Muž' },
            { id: 'f', label: 'Žena' },
          ]}
        />
      </div>

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Kontaktní údaje
      </div>

      <TextFieldElement
        control={control}
        type="email"
        name="uEmail"
        label="E-mail"
        required
      />
      <TextFieldElement
        control={control}
        type="tel"
        name="uTelefon"
        label="Telefon"
        required
      />

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Bydliště
      </div>

      <TextFieldElement control={control} name="uStreet" label="Ulice" required />

      <div className="grid grid-cols-2 gap-2">
        <TextFieldElement
          control={control}
          name="uConscriptionNumber"
          label="Č.popisné"
        />
        <TextFieldElement
          control={control}
          name="uOrientationNumber"
          label="Č.orientační"
        />
      </div>

      <TextFieldElement control={control} name="uCity" label="Město" required />
      <TextFieldElement control={control} name="uDistrict" label="Městská čtvrť" />
      <TextFieldElement control={control} name="uPostalCode" label="PSČ" />

      <div className="col-full">
        <ComboboxElement
          control={control}
          label="Národnost"
          name="uNationality"
          options={countries.map((x) => ({ id: x.code.toString(), label: x.label }))}
        />
      </div>

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Tréninkové údaje
      </div>

      <div className="col-full grid gap-2">
        <ComboboxElement
          control={control}
          label="Tréninková skupina"
          name="uSkupina"
          options={cohorts?.skupinies?.nodes?.map((x) => ({ id: x.id, label: x.sName }))}
        />

        <RichTextEditor
          initialState={data?.uPoznamky}
          control={control}
          name="uPoznamky"
          label="Poznámka"
        />

        <CheckboxElement
          control={control}
          name="uDancer"
          value="1"
          label="Aktivní tanečník"
        />
        <CheckboxElement control={control} name="uTeacher" value="1" label="Trenér" />
      </div>

      <div className="tracking-wide uppercase text-stone-700 text-xs col-full mt-4">
        Přístupy
      </div>

      <div className="col-full grid gap-2">
        <ComboboxElement
          control={control}
          label="Uživatelská role"
          name="uGroup"
          options={
            roles?.permissions?.nodes?.map((x) => ({ id: x.id, label: x.peName })) || []
          }
        />

        <CheckboxElement
          control={control}
          name="uBan"
          value="1"
          label="Neaktivní uživatel (ban)"
        />
        <CheckboxElement
          control={control}
          name="uSystem"
          value="1"
          label="Systémový uživatel"
        />
      </div>
    </form>
  );
};
