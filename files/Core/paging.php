<?php
class Paging {
    private $previousPage;
    private $currentPage;
    private $nextPage;
    private $itemsPerPage;
    private $defaultItemsPerPage;
    private $itemsPerPageField;
    private $currentPageField;
    private $pageRange;
    private $pageCount;
    private $totalItems;
    private $pagesInRange;
    private $valid;
    
    function __construct(PagingAdapterInterface $d = null) {
        if($d instanceof PagingAdapterInterface) {
            $this->adapter = $d;
            $this->totalItems = $this->adapter->count();
        }
        $this->valid = false;
        return $this;
    }
    
    function setAdapter(PagingAdapterInterface $d) {
        if(!($d instanceof PagingAdapterInterface))
            return false;
        $this->adapter = $d;
        $this->totalItems = $this->adapter->count();
        $this->recalculate();
    }
    function setCurrentPage($p) {
        if(!is_numeric($p))
            return false;
        $this->currentPage = $p;
        $this->recalculate();
    }
    function setCurrentPageField($f) {
        $this->currentPageField = $f;
        $this->recalculate();
    }
    function setItemsPerPageField($f) {
        $this->itemsPerPageField = $f;
        $this->recalculate();
    }
    function setDefaultItemsPerPage($i) {        
        if(!is_numeric($i))
            return false;
        $this->defaultItemsPerPage = $i;
        $this->recalculate();
    }
    function setItemsPerPage($p) {
        if(!is_numeric($p))
            return false;
        $this->itemsPerPage = $p;
        $this->recalculate();
    }
    function setPageRange($p) {
        if(is_numeric($p))
            $this->pageRange = $p;
        $this->recalculate();
    }
    private function recalculate() {
        if(!isset($this->adapter) ||
                (!isset($this->itemsPerPage) && !isset($this->itemsPerPageField) &&
                    !isset($this->defaultItemsPerPage)) ||
                (!isset($this->currentPage) && !isset($this->currentPageField))) {
            $this->valid = false;
            return; 
        }
        
        if(isset($this->itemsPerPageField) &&
                get($this->itemsPerPageField) !== null)
            $this->itemsPerPage = get($this->itemsPerPageField);
        elseif(isset($this->defaultItemsPerPage))
            $this->itemsPerPage = $this->defaultItemsPerPage;
        else
            $this->itemsPerPage = 20;
        
        if(get($this->currentPageField) !== null)
            $this->currentPage = get($this->currentPageField);
        else
            $this->currentPage = 1;
        
        $this->pageCount = ceil($this->totalItems / $this->itemsPerPage);
        if($this->currentPage < 1)
            $this->currentPage = 1;
        elseif($this->currentPage > $this->pageCount && $this->pageCount > 0)
            $this->currentPage = $this->pageCount;
        
        $this->previousPage = $this->currentPage > 1 ?
            $this->currentPage - 1 : 1;
        $this->nextPage = $this->currentPage < $this->pageCount ?
            $this->currentPage + 1 :
            ($this->pageCount > 0 ? $this->pageCount : 1);
        $this->pagesInRange = array();
        
        if(isset($this->pageRange)) {
            $halfRange = floor($this->pageRange / 2);
            
            if($this->pageRange > $this->pageCount)
                $this->pagesInRange = range(1, $this->pageCount);
            elseif($this->currentPage - $halfRange <= 1)
                $this->pagesInRange = range(1, $this->pageRange);
            elseif($this->currentPage + $halfRange >= $this->pageCount) {
                $this->pagesInRange = range($this->pageCount - $this->pageRange + 1, $this->pageCount);
            }
            else
                $this->pagesInRange = range($this->currentPage - $halfRange,
                    $this->currentPage + $halfRange);
        } else {
            $this->pagesInRange = range(1, $this->pageCount);
        }
        
        $this->valid = true;
        return;
    }
    
    function getPages() {
        if(!$this->valid)
            return false;
        return array(
            'first' => 1,
            'previous' => $this->previousPage,
            'current' => $this->currentPage,
            'next' => $this->nextPage,
            'last' => $this->pageCount,
            'total' => $this->totalItems,
            'pagesInRange' => $this->pagesInRange
        );
    }
    function getLink($i, $label, $perPage = null) {
        if(!$this->valid)
            return false;
        if($perPage === null)
            $perPage = $this->itemsPerPage;
        
        $p = (isset($this->currentPageField) ? $this->currentPageField : 'p');
        $c = (isset($this->itemsPerPageField) ? $this->itemsPerPageField : 'c');
        $url = http_build_query(array_merge($_GET, array(
            $c => $perPage,
            $p => $i)
        ));
        return "<a href=\"?$url\">$label</a>";
    }
    function getNavigation() {
        if($this->getPageCount() <= 1)
            return $this->getCountSetting();
        
        $out = '';
        if($this->getCurrentPage() != 1) {
            $out .= $this->getLink(1, '&lt;&lt;') . '&nbsp;';
            $out .= $this->getLink($this->getPreviousPage(), '&lt;');
        } else {
            $out .= '<span style="padding:0 2px;">&lt;&lt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&lt;</span>';
        }
        $pages = $this->getPagesInRange();
        if(end($pages) + 10 < $this->pageCount)
            array_push($pages, end($pages) + 10);
        if(reset($pages) - 10 > 1)
            array_unshift($pages, reset($pages) - 10);
        
        $out .= '&nbsp;|';
        foreach($pages as $i) {
            $out .= '<div style="text-align:center;width:3ex;display:inline-block;*display:inline;">';
            if($i != $this->getCurrentPage())
                $out .= $this->getLink($i, $i);
            else
                $out .= $i;
            $out .= '</div>|';
        }
        $out .= '&nbsp;';
        if($this->getCurrentPage() != $this->getPageCount()) {
            $out .= $this->getLink($this->getNextPage(), '&gt;') . '&nbsp;';
            $out .= $this->getLink($this->getPageCount(), '&gt;&gt;');
        } else {
            $out .= '<span style="padding:0 2px;">&gt;</span>&nbsp;';
            $out .= '<span style="padding:0 2px;">&gt;&gt;</span>';
        }
        return $out . '<br/>' . $this->getCountSetting();
    }
    function getCountSetting() {
        if($this->totalItems <= 5)
            return '';
        $options = array(5, 10, 20, 50);
        
        $out = 'zobrazit ';
        if($this->itemsPerPage === $this->totalItems)
            $out .= '<span style="padding:0 2px;">vše</span>';
        else
            $out .= $this->getLink(1, 'vše', $this->totalItems);
        
        foreach($options as $option) {
            if($option > $this->totalItems)
                continue;
            
            if($this->itemsPerPage == $option) {
                $out .= '&nbsp;|&nbsp;<span style="padding:0 2px;">' . $option . '</span>';
            } else {
                $i = floor($this->itemsPerPage * ($this->currentPage - 1) / $option) + 1;
                $out .= '&nbsp;|&nbsp;' . $this->getLink($i, $option, $option);
            }
        }
        $out .= ' položek na stránku';
        return $out;
    }
    function getPreviousPage() {
        if(!$this->valid)
            return false;
        return $this->previousPage;
    }
    function getCurrentPage() {
        if(!$this->valid)
            return false;
        return $this->currentPage;
    }
    function getNextPage() {
        if(!$this->valid)
            return false;
        return $this->nextPage;
    }
    function getPageCount() {
        if(!$this->valid)
            return false;
        return $this->pageCount;
    }
    function getItemsPerPage() {
        if(!$this->valid)
            return false;
        return $this->itemsPerPage;
    }
    function getTotalItems() {
        if(!$this->valid)
            return false;
        return $this->totalItems;
    }
    function getPagesInRange() {
        if(!$this->valid)
            return false;
        return $this->pagesInRange;
    }
    function getItems() {
        if(!$this->valid)
            return array();
        
        $offset = $this->itemsPerPage * ($this->currentPage - 1);
        return $this->adapter->page($offset, $this->itemsPerPage);
    }
}
?>