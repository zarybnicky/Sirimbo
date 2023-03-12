import { AkceInput } from 'lib/graphql';
import { EventFragment, useCreateEventMutation, useUpdateEventMutation } from 'lib/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook'
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { SlateEditorElement } from './Slate';

type FormProps = Pick<AkceInput, 'aJmeno' | 'aKde' | 'summary' | 'aInfo' | 'aOd' | 'aDo' |
  'aKapacita' | 'aVisible' | 'isPublic' | 'enableNotes' | 'aLock'>;

export const EventForm: React.FC<{
  data?: EventFragment;
  onSuccess?: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateEventMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateEventMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      aJmeno: data?.aJmeno,
      aKde: data?.aKde,
      summary: data?.summary,
      aInfo: data?.aInfo,
      aOd: data?.aOd,
      aDo: data?.aDo,
      aKapacita: data?.aKapacita,
      aVisible: data?.aVisible,
      isPublic: data?.isPublic,
      enableNotes: data?.enableNotes,
      aLock: data?.aLock,
    });
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      ...values,
      aInfo: JSON.stringify(values.aInfo),
      aDokumenty: '',
    };
    if (data) {
      await doUpdate({ id: data.id, patch });
    } else {
      await doCreate({ input: patch });
    }
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="aJmeno" label="Název" required />
      <TextFieldElement control={control} name="aKde" label="Místo akce" required />
      <SlateEditorElement control={control} name="summary" label="Shrnutí" />
      <SlateEditorElement control={control} name="aInfo" label="Další info" />
      <TextFieldElement control={control} type="date" label="Od" name="aOd" required />
      <TextFieldElement type="date" helperText="(pokud je prázdné, počítá se jako 'Od')" control={control} label="Do" name="aDo" required />
      <TextFieldElement control={control} type="number" name="aKapacita" label="Kapacita" required />
      <CheckboxElement control={control} name="aVisible" value="1" label="Zviditelnit pro členy" />
      <CheckboxElement control={control} name="isPublic" value="1" label="Zviditelnit pro veřejnost" />
      <CheckboxElement control={control} name="enableNotes" value="1" label="Povolit poznámky k přihlášce" />
      <CheckboxElement control={control} name="aLock" value="1" label="Uzamčená" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
