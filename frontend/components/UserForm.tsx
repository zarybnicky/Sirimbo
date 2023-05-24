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
import { UserInput } from 'lib/graphql';
import { UserFragment } from 'lib/graphql/CurrentUser';
import { useQueryClient } from '@tanstack/react-query';
import dynamic from 'next/dynamic';
import { getGqlKey, useGqlMutation, useGqlQuery } from 'lib/query';
import { RoleListDocument } from 'lib/graphql/Roles';
import { CohortListDocument } from 'lib/graphql/Cohorts';
import {
  CreateUserDocument,
  UpdateUserDocument,
  UserListDocument,
} from 'lib/graphql/User';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<
  UserInput,
  | 'uLogin'
  | 'uJmeno'
  | 'uPrijmeni'
  | 'uNarozeni'
  | 'uRodneCislo'
  | 'uPohlavi'
  | 'uEmail'
  | 'uTelefon'
  | 'uStreet'
  | 'uConscriptionNumber'
  | 'uOrientationNumber'
  | 'uCity'
  | 'uDistrict'
  | 'uPostalCode'
  | 'uNationality'
  | 'uPoznamky'
  | 'uDancer'
  | 'uTeacher'
  | 'uBan'
  | 'uLock'
  | 'uSystem'
  | 'uGroup'
  | 'uSkupina'
  | 'uPass'
>;

export const UserForm: React.FC<{
  data?: UserFragment;
}> = ({ data }) => {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(getGqlKey(UserListDocument, {}));
  }, [queryClient]);

  const { mutateAsync: doCreate } = useGqlMutation(CreateUserDocument, { onSuccess });
  const { mutateAsync: doUpdate } = useGqlMutation(UpdateUserDocument, { onSuccess });

  const countries = useCountries();
  const { data: roles } = useGqlQuery(RoleListDocument, {});
  const { data: cohorts } = useGqlQuery(CohortListDocument, {});

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      uLogin: data?.uLogin,
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
    });
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const { uLogin: _uLogin, ...patch } = values;
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({
        input: {
          ...patch,
          uLogin: _uLogin.toLowerCase(),
          uLock: false,
        },
      });
    }
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      {data ? (
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
        validation={{
          pattern: {
            value: /[0-9]{9,10}/,
            message: 'Neplatné rodné číslo',
          },
        }}
      />

      <div>
        <RadioButtonGroupElement
          control={control}
          name="uPohlavi"
          required
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
          required
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

      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
