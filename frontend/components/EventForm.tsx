import { EventInput } from 'lib/graphql';
import {
  EventFragment,
  useCreateEventMutation,
  useUpdateEventMutation,
} from 'lib/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import dynamic from 'next/dynamic';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

type FormProps = Pick<
  EventInput,
  | 'name'
  | 'locationText'
  | 'summary'
  | 'description'
  | 'since'
  | 'until'
  | 'capacity'
  | 'isVisible'
  | 'isPublic'
  | 'enableNotes'
  | 'isLocked'
>;

export const EventForm: React.FC<{
  data?: EventFragment;
  onSuccess?: () => void;
}> = ({ data, onSuccess }) => {
  const { mutateAsync: doCreate } = useCreateEventMutation({ onSuccess });
  const { mutateAsync: doUpdate } = useUpdateEventMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      name: data?.name,
      locationText: data?.locationText,
      summary: data?.summary,
      description: data?.description,
      since: data?.since,
      until: data?.until,
      capacity: data?.capacity,
      isVisible: data?.isVisible,
      isPublic: data?.isPublic,
      enableNotes: data?.enableNotes,
      isLocked: data?.isLocked,
    });
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      ...values,
      filesLegacy: '',
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
      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement
        control={control}
        name="locationText"
        label="Místo akce"
        required
      />
      <RichTextEditor
        control={control}
        initialState={data?.summary}
        name="summary"
        label="Shrnutí"
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Další info"
      />
      <TextFieldElement control={control} type="date" label="Od" name="since" required />
      <TextFieldElement
        type="date"
        helperText="(pokud je prázdné, počítá se jako 'Od')"
        control={control}
        label="Do"
        name="until"
        required
      />
      <TextFieldElement
        control={control}
        type="number"
        name="capacity"
        label="Kapacita"
        required
      />
      <CheckboxElement
        control={control}
        name="isVisible"
        value="1"
        label="Zviditelnit pro členy"
      />
      <CheckboxElement
        control={control}
        name="isPublic"
        value="1"
        label="Zviditelnit pro veřejnost"
      />
      <CheckboxElement
        control={control}
        name="enableNotes"
        value="1"
        label="Povolit poznámky k přihlášce"
      />
      <CheckboxElement control={control} name="isLocked" value="1" label="Uzamčená" />
      <SubmitButton loading={onSubmit.loading} />
    </form>
  );
};
