import { useQueryClient } from '@tanstack/react-query';
import { useAuth } from 'lib/data/use-auth';
import React from 'react';
import { useForm } from 'react-hook-form';
import { ComboboxElement } from 'components/Combobox';
import { RadioButtonGroupElement } from 'components/RadioButtomGroupElement';
import { TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { useCountries } from 'lib/data/use-countries';
import { UserInput } from 'lib/graphql';
import { ErrorBox } from 'components/ErrorBox';
import { SubmitButton } from './SubmitButton';
import { pipe } from 'fp-ts/lib/function';
import { pick } from 'lib/form-utils';
import { getGqlKey, useGqlMutation } from 'lib/query';
import { CurrentUserDocument } from 'lib/graphql/CurrentUser';
import { UpdateUserDocument } from 'lib/graphql/User';

const fields = [
  'uJmeno',
  'uPrijmeni',
  'uNarozeni',
  'uRodneCislo',
  'uPohlavi',
  'uEmail',
  'uTelefon',
  'uStreet',
  'uConscriptionNumber',
  'uOrientationNumber',
  'uCity',
  'uDistrict',
  'uPostalCode',
  'uNationality',
  'uPoznamky',
] as const;
type FormProps = Pick<UserInput, (typeof fields)[number]>;

export const PersonalInfoForm: React.FC<{
  onSuccess: () => void;
}> = ({ onSuccess: realOnSuccess }) => {
  const { user } = useAuth();
  const countries = useCountries();

  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(getGqlKey(CurrentUserDocument, {}));
    realOnSuccess();
  }, [queryClient, realOnSuccess]);

  const { mutateAsync: doUpdate } = useGqlMutation(UpdateUserDocument, { onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (user) {
      reset(pipe(user, pick(fields)));
    }
  }, [reset, user]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpdate({ id: user?.id!, patch: values });
  });

  return (
    <form className="grid lg:grid-cols-2 gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />

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
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
