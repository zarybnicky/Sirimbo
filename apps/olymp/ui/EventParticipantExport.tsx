import * as React from 'react';
import { saveAs } from 'file-saver';
import { EventRegistrantsDocument } from '@app/graphql/Event';
import { useQuery } from 'urql';
import { buttonCls } from '@app/ui/style';

export function EventParticipantExport({ id }: { id: string }) {
  const [{ data }] = useQuery({query: EventRegistrantsDocument, variables: { id }, pause: !!id});

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
        { header: 'Skupiny', key: 'cohorts' },
      ];

      worksheet.getRow(1).font = { bold: true };
      worksheet.columns.forEach((column) => {
        column.width = (column?.header?.length || 0) + 10;
        column.alignment = { horizontal: 'center' };
      });

      data.event?.registrantsList?.forEach((x) =>
        worksheet.addRow({
          firstName: x.firstName,
          lastName: x.firstName,
          birthNumber: x.nationalIdNumber,
          phone: x.primaryPhone,
          email: x.primaryEmail,
          cohorts: x.cohortMembershipsList.map(x => x.cohort?.sName).join(', '),
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
