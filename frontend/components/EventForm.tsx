import { EventFragment, AkceInput, useCreateEventMutation, useUpdateEventMutation } from 'lib/graphql';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { DatePickerElement } from 'react-hook-form-mui';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';

type FormProps = Pick<AkceInput, 'aJmeno' | 'aKde' | 'aInfo' | 'aOd' | 'aDo' |
  'aKapacita' | 'aVisible' | 'aLock'>;

export const EventForm: React.FC<{
  data?: EventFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateEventMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateEventMutation({ onSuccess });

  const { control, handleSubmit, formState } = useForm<FormProps>({
    defaultValues: {
      aJmeno: data?.aJmeno,
      aKde: data?.aKde,
      aInfo: data?.aInfo,
      aOd: data?.aOd,
      aDo: data?.aDo,
      aKapacita: data?.aKapacita,
      aVisible: data?.aVisible,
      aLock: data?.aLock,
    },
  });

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    if (data) {
      await doUpdate({ id: data.aId, patch: values });
    } else {
      await doCreate({ input: { ...values, aDokumenty: '' } });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="aJmeno" label="Název" required />
      <TextFieldElement control={control} name="aKde" label="Místo akce" required />
      <TextAreaElement control={control} name="aInfo" label="Další info" rows={3} required />
      <DatePickerElement inputProps={{ fullWidth: true }} control={control} label="Od" name="aOd" required />
      <DatePickerElement inputProps={{ fullWidth: true }} helperText="(pokud je prázdné, počítá se jako 'Od')" control={control} label="Do" name="aDo" required />
      <TextFieldElement control={control} type="number" name="aKapacita" label="Kapacita" required />
      <CheckboxElement control={control} name="aVisible" value="1" label="Zviditelnit" />
      <CheckboxElement control={control} name="aLock" value="1" label="Uzamčená" />
      <SubmitButton loading={onSubmit.loading} disabled={!formState.isValid} />
    </form>
  );
};
