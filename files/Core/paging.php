<?php
class Paging
{
    private $_previousPage;
    private $_currentPage;
    private $_nextPage;
    private $_itemsPerPage;
    private $_defaultItemsPerPage;
    private $_itemsPerPageField;
    private $_currentPageField;
    private $_pageRange;
    private $_pageCount;
    private $_totalItems;
    private $_pagesInRange;
    private $_valid;
    private $_adapter;

    function __construct(PagingAdapterInterface $d = null) {
        if ($d instanceof PagingAdapterInterface) {
            $this->_adapter = $d;
            $this->_totalItems = $this->_adapter->count();
        }
        $this->_valid = false;
        return $this;
    }

    function setAdapter(PagingAdapterInterface $d) {
        if (!($d instanceof PagingAdapterInterface))
            return false;
        $this->_adapter = $d;
        $this->_totalItems = $this->_adapter->count();
        $this->_recalculate();
    }
    function setCurrentPage($p) {
        if (!is_numeric($p))
            return false;
        $this->_currentPage = $p;
        $this->_recalculate();
    }
    function setCurrentPageField($f) {
        $this->_currentPageField = $f;
        $this->_recalculate();
    }
    function setItemsPerPageField($f) {
        $this->_itemsPerPageField = $f;
        $this->_recalculate();
    }
    function setDefaultItemsPerPage($i) {
        if (!is_numeric($i))
            return false;
        $this->_defaultItemsPerPage = $i;
        $this->_recalculate();
    }
    function setItemsPerPage($p) {
        if (!is_numeric($p))
            return false;
        $this->_itemsPerPage = $p;
        $this->_recalculate();
    }
    function setPageRange($p) {
        if (is_numeric($p))
            $this->_pageRange = $p;
        $this->_recalculate();
    }
    private function _recalculate() {
        if (!isset($this->_adapter)
            || (!isset($this->_itemsPerPage) && !isset($this->_itemsPerPageField)
            && !isset($this->_defaultItemsPerPage))
            || (!isset($this->_currentPage) && !isset($this->_currentPageField))) {
            $this->_valid = false;
            return;
        }

        if (isset($this->_itemsPerPageField)
            && get($this->_itemsPerPageField) !== null)
            $this->_itemsPerPage = get($this->_itemsPerPageField);
        elseif (isset($this->_defaultItemsPerPage))
            $this->_itemsPerPage = $this->_defaultItemsPerPage;
        else
            $this->_itemsPerPage = 20;

        if (get($this->_currentPageField) !== null)
            $this->_currentPage = get($this->_currentPageField);
        else
            $this->_currentPage = 1;

        $this->_pageCount = ceil($this->_totalItems / $this->_itemsPerPage);
        if ($this->_currentPage < 1)
            $this->_currentPage = 1;
        elseif ($this->_currentPage > $this->_pageCount && $this->_pageCount > 0)
            $this->_currentPage = $this->_pageCount;

        $this->_previousPage = $this->_currentPage > 1 ?
            $this->_currentPage - 1 : 1;
        $this->_nextPage = $this->_currentPage < $this->_pageCount ?
            $this->_currentPage + 1 :
            ($this->_pageCount > 0 ? $this->_pageCount : 1);
        $this->_pagesInRange = array();

        if (isset($this->_pageRange)) {
            $halfRange = floor($this->_pageRange / 2);

            if ($this->_pageRange > $this->_pageCount)
                $this->_pagesInRange = range(1, $this->_pageCount);
            elseif ($this->_currentPage - $halfRange <= 1)
                $this->_pagesInRange = range(1, $this->_pageRange);
            elseif ($this->_currentPage + $halfRange >= $this->_pageCount) {
                $this->_pagesInRange = range($this->_pageCount - $this->_pageRange + 1, $this->_pageCount);
            }
            else
                $this->_pagesInRange = range($this->_currentPage - $halfRange,
                    $this->_currentPage + $halfRange);
        } else {
            $this->_pagesInRange = range(1, $this->_pageCount);
        }

        $this->_valid = true;
        return;
    }

    function getPages() {
        if (!$this->_valid)
            return false;
        return array(
            'first' => 1,
            'previous' => $this->_previousPage,
            'current' => $this->_currentPage,
            'next' => $this->_nextPage,
            'last' => $this->_pageCount,
            'total' => $this->_totalItems,
            'pagesInRange' => $this->_pagesInRange
        );
    }
    function getLink($i, $label, $perPage = null) {
        if (!$this->_valid)
            return false;
        if ($perPage === null)
            $perPage = $this->_itemsPerPage;

        $p = (isset($this->_currentPageField) ? $this->_currentPageField : 'p');
        $c = (isset($this->_itemsPerPageField) ? $this->_itemsPerPageField : 'c');
        $url = http_build_query(array_merge($_GET, array(
            $c => $perPage,
            $p => $i)
        ));
        return "<a href=\"?$url\">$label</a>";
    }
    function getNavigation() {
        if ($this->getPageCount() <= 1)
            return $this->getCountSetting();

        $out = '';
        if ($this->getCurrentPage() != 1) {
            $out .= $this->getLink(1, '&lt;&lt;') . '&nbsp;';
            $out .= $this->getLink($this->getPreviousPage(), '&lt;');
        } else {
            $out .= '<span style="padding:0 2px;">&lt;&lt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&lt;</span>';
        }
        $pages = $this->getPagesInRange();
        if (end($pages) + 10 < $this->_pageCount)
            array_push($pages, end($pages) + 10);
        if (reset($pages) - 10 > 1)
            array_unshift($pages, reset($pages) - 10);

        $out .= '&nbsp;|';
        foreach ($pages as $i) {
            $out .= '<div style="text-align:center;width:3ex;display:inline-block;*display:inline;">';
            if ($i != $this->getCurrentPage())
                $out .= $this->getLink($i, $i);
            else
                $out .= $i;
            $out .= '</div>|';
        }
        $out .= '&nbsp;';
        if ($this->getCurrentPage() != $this->getPageCount()) {
            $out .= $this->getLink($this->getNextPage(), '&gt;') . '&nbsp;';
            $out .= $this->getLink($this->getPageCount(), '&gt;&gt;');
        } else {
            $out .= '<span style="padding:0 2px;">&gt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&gt;&gt;</span>';
        }
        return $out . '<br/>' . $this->getCountSetting();
    }
    function getCountSetting() {
        if ($this->_totalItems <= 5)
            return '';
        $options = array(5, 10, 20, 50);

        $out = 'zobrazit ';
        if ($this->_itemsPerPage === $this->_totalItems)
            $out .= '<span style="padding:0 2px;">vše</span>';
        else
            $out .= $this->getLink(1, 'vše', $this->_totalItems);

        foreach ($options as $option) {
            if ($option > $this->_totalItems)
                continue;

            if ($this->_itemsPerPage == $option) {
                $out .= '&nbsp;|&nbsp;<span style="padding:0 2px;">' . $option . '</span>';
            } else {
                $i = floor($this->_itemsPerPage * ($this->_currentPage - 1) / $option) + 1;
                $out .= '&nbsp;|&nbsp;' . $this->getLink($i, $option, $option);
            }
        }
        $out .= ' položek na stránku';
        return $out;
    }
    function getPreviousPage() {
        if (!$this->_valid)
            return false;
        return $this->_previousPage;
    }
    function getCurrentPage() {
        if (!$this->_valid)
            return false;
        return $this->_currentPage;
    }
    function getNextPage() {
        if (!$this->_valid)
            return false;
        return $this->_nextPage;
    }
    function getPageCount() {
        if (!$this->_valid)
            return false;
        return $this->_pageCount;
    }
    function getItemsPerPage() {
        if (!$this->_valid)
            return false;
        return $this->_itemsPerPage;
    }
    function getTotalItems() {
        if (!$this->_valid)
            return false;
        return $this->_totalItems;
    }
    function getPagesInRange() {
        if (!$this->_valid)
            return false;
        return $this->_pagesInRange;
    }
    function getItems() {
        if (!$this->_valid)
            return array();

        $offset = $this->_itemsPerPage * ($this->_currentPage - 1);
        return $this->_adapter->page($offset, $this->_itemsPerPage);
    }
}
?>