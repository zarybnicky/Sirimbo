import { EventInput } from '@app/graphql';
import {
  CreateEventDocument,
  DeleteEventDocument,
  EventDocument,
  UpdateEventDocument,
} from '@app/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from '@app/ui/fields/checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useRouter } from 'next/router';
import { DeleteButton } from './DeleteButton';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { RichTextEditor } from '@app/ui/fields/richtext';
import { TitleBar } from './TitleBar';
import { RadioButtonGroupElement } from './RadioButtomGroupElement';
import { AdminEntity } from './generic/AdminEntityList';
import { ComboboxElement } from './Combobox';

type FormProps = Pick<
  EventInput,
  | 'name'
  | 'type'
  | 'locationText'
  | 'summary'
  | 'description'
  | 'descriptionMember'
  | 'capacity'
  | 'isVisible'
  | 'isPublic'
  | 'enableNotes'
  | 'isLocked'
  | 'titleImageLegacy'
>;

export const EventForm = ({ entity, id = '' }: { entity: AdminEntity; id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({query: EventDocument, variables: { id }, pause: !id });
  const data = query.data?.event;
  const title = id ? data?.name || '(Bez názvu)' : 'Nová akce';

  const create = useMutation(CreateEventDocument)[1];
  const update = useMutation(UpdateEventDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (data) {
      reset({
        name: data?.name,
        type: data?.type,
        locationText: data?.locationText,
        summary: data?.summary,
        description: data?.description,
        capacity: data?.capacity,
        isVisible: data?.isVisible,
        isPublic: data?.isPublic,
        isLocked: data?.isLocked,
        enableNotes: data?.enableNotes,
      });
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      name: values.name,
      type: values.type,
      locationText: values.locationText,
      summary: values.summary,
      description: values.description,
      capacity: values.capacity,
      isVisible: values.isVisible,
      isPublic: values.isPublic,
      isLocked: values.isLocked,
      enableNotes: values.enableNotes,
    };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data!.createEvent?.event?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace(entity.editRoute(id));
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.event === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar title={title} >
        {id && (
          <DeleteButton
            doc={DeleteEventDocument}
            id={id}
            title="smazat akci"
            redirect={entity.listRoute}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <ComboboxElement
        control={control}
        name="type"
        label="Typ události"
        placeholder="vyberte typ"
        options={[
          {id: 'camp', label: `Soustředění`},
          {id: 'lesson', label: `Lekce`},
          {id: 'reservation', label: `Nabídka`},
          {id: 'holiday', label: `Prázdniny`},
        ]}
      />
      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement
        control={control}
        name="locationText"
        label="Místo akce"
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
      <CheckboxElement control={control} name="isLocked" value="1" label="Uzamčená" />
      <CheckboxElement
        control={control}
        name="enableNotes"
        value="1"
        label="Povolit poznámky k přihlášce"
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
      <RichTextEditor
        control={control}
        initialState={data?.descriptionMember}
        name="descriptionMember"
        label="Další info pro členy"
      />

      <RadioButtonGroupElement
        control={control}
        name="titleImageLegacy"
        options={[
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671943-Akce-titulni1.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671943-Akce-titulni1.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni2.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni2.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni3.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni3.jpg" />
          },
          {
            id: 'https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni4.jpg',
            label: <img alt="" src="https://files.rozpisovnik.cz/file/rozpisovnik/tkolymp/1687515671944-Akce-titulni4.jpg" />
          },
        ]}
      />
    </form>
  );
};