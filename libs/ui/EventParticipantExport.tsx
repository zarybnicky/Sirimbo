import * as React from 'react';
import { saveAs } from 'file-saver';
import { EventDocument } from '@app/graphql/Event';
import { useQuery } from 'urql';
import { buttonCls } from './style/button';

export function EventParticipantExport({ id }: { id: string }) {
  const [{ data }] = useQuery({query: EventDocument, variables: { id }});

  const saveData = React.useCallback(
    async (e?: React.MouseEvent) => {
      e?.preventDefault();
      if (!data) {
        return;
      }
      const { Workbook } = await import('exceljs');
      const workbook = new Workbook();
      const worksheet = workbook.addWorksheet(data.event?.name || 'Sheet 1');

      worksheet.columns = [
        { header: 'Jméno', key: 'firstName' },
        { header: 'Přijmení', key: 'lastName' },
        { header: 'Rodné číslo', key: 'birthNumber' },
        { header: 'Telefon', key: 'phone' },
        { header: 'E-mail', key: 'email' },
      ];

      worksheet.getRow(1).font = { bold: true };
      worksheet.columns.forEach((column) => {
        column.width = (column?.header?.length || 0) + 10;
        column.alignment = { horizontal: 'center' };
      });

      data.event?.eventRegistrationsList.forEach((x) =>
        worksheet.addRow({
          firstName: x.person?.firstName,
          lastName: x.person?.firstName,
          birthNumber: x.person?.nationalIdNumber,
          phone: x.person?.primaryPhone,
          email: x.person?.primaryEmail,
        }),
      );

      const buf = await workbook.xlsx.writeBuffer();
      saveAs(new Blob([buf]), `${data.event?.name || 'export-akce'}.xlsx`);
    },
    [data],
  );

  return <button type="button" className={buttonCls({ variant: 'outline' })} onClick={saveData}>
    Export přihlášených
  </button>;
}
