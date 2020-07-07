pragma solidity ^0.5.0;
pragma experimental ABIEncoderV2;
library SafeMath {
    /**
     * @dev Returns the addition of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `+` operator.
     *
     * Requirements:
     * - Addition cannot overflow.
     */
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c >= a, "SafeMath: addition overflow");

        return c;
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting on
     * overflow (when the result is negative).
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     * - Subtraction cannot overflow.
     */
    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        return sub(a, b, "SafeMath: subtraction overflow");
    }

    /**
     * @dev Returns the subtraction of two unsigned integers, reverting with custom message on
     * overflow (when the result is negative).
     *
     * Counterpart to Solidity's `-` operator.
     *
     * Requirements:
     * - Subtraction cannot overflow.
     *
     * _Available since v2.4.0._
     */
    function sub(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b <= a, errorMessage);
        uint256 c = a - b;

        return c;
    }

    /**
     * @dev Returns the multiplication of two unsigned integers, reverting on
     * overflow.
     *
     * Counterpart to Solidity's `*` operator.
     *
     * Requirements:
     * - Multiplication cannot overflow.
     */
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        // Gas optimization: this is cheaper than requiring 'a' not being zero, but the
        // benefit is lost if 'b' is also tested.
        // See: https://github.com/OpenZeppelin/openzeppelin-contracts/pull/522
        if (a == 0) {
            return 0;
        }

        uint256 c = a * b;
        require(c / a == b, "SafeMath: multiplication overflow");

        return c;
    }

    /**
     * @dev Returns the integer division of two unsigned integers. Reverts on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     */
    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        return div(a, b, "SafeMath: division by zero");
    }

    /**
     * @dev Returns the integer division of two unsigned integers. Reverts with custom message on
     * division by zero. The result is rounded towards zero.
     *
     * Counterpart to Solidity's `/` operator. Note: this function uses a
     * `revert` opcode (which leaves remaining gas untouched) while Solidity
     * uses an invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     *
     * _Available since v2.4.0._
     */
    function div(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        // Solidity only automatically asserts when dividing by 0
        require(b > 0, errorMessage);
        uint256 c = a / b;
        // assert(a == b * c + a % b); // There is no case in which this doesn't hold

        return c;
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * Reverts when dividing by zero.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     */
    function mod(uint256 a, uint256 b) internal pure returns (uint256) {
        return mod(a, b, "SafeMath: modulo by zero");
    }

    /**
     * @dev Returns the remainder of dividing two unsigned integers. (unsigned integer modulo),
     * Reverts with custom message when dividing by zero.
     *
     * Counterpart to Solidity's `%` operator. This function uses a `revert`
     * opcode (which leaves remaining gas untouched) while Solidity uses an
     * invalid opcode to revert (consuming all remaining gas).
     *
     * Requirements:
     * - The divisor cannot be zero.
     *
     * _Available since v2.4.0._
     */
    function mod(uint256 a, uint256 b, string memory errorMessage) internal pure returns (uint256) {
        require(b != 0, errorMessage);
        return a % b;
    }
}
contract Ownable {
  address public owner;

  event transferOwner(address indexed existingOwner, address indexed newOwner);

  constructor() public {
    owner = msg.sender;
  }

  modifier onlyOwner() {
    require(msg.sender == owner);
    _;
  }

  function transferOwnership(address newOwner) onlyOwner public {
    if (newOwner != address(0)) {
      owner = newOwner;
      emit transferOwner(msg.sender, owner);
    }
  }
}

contract ERC20Basic {
  function totalSupply() public view returns (uint256);
  function balanceOf(address who) public view returns (uint256);
  function transfer(address to, uint256 value) public returns (bool);
  event Transfer(address indexed from, address indexed to, uint256 value);
}

contract ERC20 is ERC20Basic {
  function allowance(address owner, address spender) public view returns (uint256);
  function transferFrom(address from, address to, uint256 value) public returns (bool);
  function approve(address spender, uint256 value) public returns (bool);
  event Approval(address indexed owner, address indexed spender, uint256 value);
}

contract BasicToken is ERC20Basic {
  using SafeMath for uint256;

  mapping(address => uint256) balances;

  uint256 totalSupply_;

  /**
  * @dev total number of tokens in existence
  */
  function totalSupply() public view returns (uint256) {
    return totalSupply_;
  }

  /**
  * @dev transfer token for a specified address
  * @param _to The address to transfer to.
  * @param _value The amount to be transferred.
  */
  function transfer(address _to, uint256 _value) public returns (bool) {
    require(_to != address(0));
    require(_value <= balances[msg.sender]);

    balances[msg.sender] = balances[msg.sender].sub(_value);
    balances[_to] = balances[_to].add(_value);
    emit Transfer(msg.sender, _to, _value);
    return true;
  }

  /**
  * @dev Gets the balance of the specified address.
  * @param _owner The address to query the the balance of.
  * @return An uint256 representing the amount owned by the passed address.
  */
  function _balanceOf(address _owner) public view returns (uint256) {
    return balances[_owner];
  }

}

contract StandardToken is ERC20, BasicToken {

  mapping (address => mapping (address => uint256)) internal allowed;


  /**
   * @dev Transfer tokens from one address to another
   * @param _from address The address which you want to send tokens from
   * @param _to address The address which you want to transfer to
   * @param _value uint256 the amount of tokens to be transferred
   */
  function transferFrom(address _from, address _to, uint256 _value) public returns (bool) {
    require(_to != address(0));
    require(_value <= balances[_from]);
    require(_value <= allowed[_from][msg.sender]);

    balances[_from] = balances[_from].sub(_value);
    balances[_to] = balances[_to].add(_value);
    allowed[_from][msg.sender] = allowed[_from][msg.sender].sub(_value);
    emit Transfer(_from, _to, _value);
    return true;
  }
 /**
   * @dev Approve the passed address to spend the specified amount of tokens on behalf of msg.sender.
   *
   * Beware that changing an allowance with this method brings the risk that someone may use both the old
   * and the new allowance by unfortunate transaction ordering. One possible solution to mitigate this
   * race condition is to first reduce the spender's allowance to 0 and set the desired value afterwards:
   * https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
   * @param _spender The address which will spend the funds.
   * @param _value The amount of tokens to be spent.
   */
  function approve(address _spender, uint256 _value) public returns (bool) {
    allowed[msg.sender][_spender] = _value;
    emit Approval(msg.sender, _spender, _value);
    return true;
  }
 /**
     * @dev Sets `amount` as the allowance of `spender` over the `owner`s tokens.
     *
     * This is internal function is equivalent to `approve`, and can be used to
     * e.g. set automatic allowances for certain subsystems, etc.
     *
     * Emits an {Approval} event.
     *
     * Requirements:
     *
     * - `owner` cannot be the zero address.
     * - `spender` cannot be the zero address.
     */
    function _approve(address owner, address spender, uint256 amount) public returns(bool){
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");
        allowed[owner][spender] = amount;
        emit Approval(owner, spender, amount);
        return true;
    }
  /**
   * @dev Function to check the amount of tokens that an owner allowed to a spender.
   * @param _owner address The address which owns the funds.
   * @param _spender address The address which will spend the funds.
   * @return A uint256 specifying the amount of tokens still available for the spender.
   */
  function allowance(address _owner, address _spender) public view returns (uint256) {
    return allowed[_owner][_spender];
  }
   /**
   * @dev Internal function that burns an amount of the token of a given
   * account.
   * @param account The account whose tokens will be burnt.
   * @param amount The amount that will be burnt.
   */
  function burn(address account, uint256 amount) internal {
    require(account != address(0));
    require(amount <= balances[account]);

    totalSupply_ = totalSupply_.sub(amount);
    balances[account] = balances[account].sub(amount);
    emit Transfer(account, address(0), amount);
  }

  /**
   * @dev Internal function that burns an amount of the token of a given
   * account, deducting from the sender's allowance for said account. Uses the
   * internal burn function.
   * @param account The account whose tokens will be burnt.
   * @param amount The amount that will be burnt.
   */
  function burnFrom(address account, uint256 amount)public {
    require(amount <= allowed[account][msg.sender]);

    // Should https://github.com/OpenZeppelin/zeppelin-solidity/issues/707 be accepted,
    // this function needs to emit an event with the updated approval.
    allowed[account][msg.sender] = allowed[account][msg.sender].sub(
      amount);
    burn(account, amount);
  }
  /**
   * @dev Increase the amount of tokens that an owner allowed to a spender.
   *
   * approve should be called when allowed[_spender] == 0. To increment
   * allowed value is better to use this function to avoid 2 calls (and wait until
   * the first transaction is mined)
   * From MonolithDAO Token.sol
   * @param _spender The address which will spend the funds.
   * @param _addedValue The amount of tokens to increase the allowance by.
   */
  function increaseApproval(address _spender, uint _addedValue) public returns (bool) {
    allowed[msg.sender][_spender] = allowed[msg.sender][_spender].add(_addedValue);
    emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
    return true;
  }

  /**
   * @dev Decrease the amount of tokens that an owner allowed to a spender.
   *
   * approve should be called when allowed[_spender] == 0. To decrement
   * allowed value is better to use this function to avoid 2 calls (and wait until
   * the first transaction is mined)
   * From MonolithDAO Token.sol
   * @param _spender The address which will spend the funds.
   * @param _subtractedValue The amount of tokens to decrease the allowance by.
   */
  function decreaseApproval(address _spender, uint _subtractedValue) public returns (bool) {
    uint oldValue = allowed[msg.sender][_spender];
    if (_subtractedValue > oldValue) {
      allowed[msg.sender][_spender] = 0;
    } else {
      allowed[msg.sender][_spender] = oldValue.sub(_subtractedValue);
    }
    emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
    return true;
  }

}

contract ERC865 {

    function transferPreSigned(
        bytes  memory _signature,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function approvePreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function increaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _addedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);

    function decreaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _subtractedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool);
}

contract ERC865Token is ERC865, StandardToken, Ownable {

    /* Nonces of transfers performed */
    mapping(bytes => bool) signatures;
    /* mapping of nonces of each user */
    mapping (address => uint256) nonces;
    
    event TransferPreSigned_Transfer_Fee(address indexed from, address indexed to, uint256 value);
    event TransferPreSigned(address indexed from, address indexed to, address indexed delegate, uint256 amount, uint256 fee);
    event ApprovalPreSigned(address indexed from, address indexed to, address indexed delegate, uint256 amount, uint256 fee);

    bytes4 internal constant transferSig = 0x48664c16;
    bytes4 internal constant approvalSig = 0xf7ac9c2e;
    bytes4 internal constant increaseApprovalSig = 0xa45f71ff;
    bytes4 internal constant decreaseApprovalSig = 0x59388d78;

    //return nonce using function
    function getNonce(address _owner) public view returns (uint256 nonce){
      return nonces[_owner];
    }

    /**
     * @notice Submit a presigned transfer
     * @param _signature bytes The signature, issued by the owner.
     * @param _to address The address which you want to transfer to.
     * @param _value uint256 The amount of tokens to be transferred.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
     //this 'to' is sending token destination , not deligate address
    function transferPreSigned(
        bytes memory _signature,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_to != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), transferSig, _to, _value, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(_value.add(_fee) <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        balances[from] = balances[from].sub(_value).sub(_fee);
        balances[_to] = balances[_to].add(_value);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Transfer(from, _to, _value);
        emit TransferPreSigned_Transfer_Fee(from, msg.sender, _fee);
        emit TransferPreSigned(from, _to, msg.sender, _value, _fee);
        return true;
    }

    /**
     * @notice Submit a presigned approval
     * @param _signature bytes The signature, issued by the owner.
     * @param _spender address The address which will spend the funds.
     * @param _value uint256 The amount of tokens to allow.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function approvePreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), approvalSig, _spender, _value, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(_value.add(_fee) <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        allowed[from][_spender] =_value;
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, _value);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, _value, _fee);
        return true;
    }

    /**
     * @notice Increase the amount of tokens that an owner allowed to a spender.
     * @param _signature bytes The signature, issued by the owner.
     * @param _spender address The address which will spend the funds.
     * @param _addedValue uint256 The amount of tokens to increase the allowance by.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function increaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _addedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), increaseApprovalSig, _spender, _addedValue, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        require(allowed[from][_spender].add(_addedValue).add(_fee) <= balances[from]);
        //require(_addedValue <= allowed[from][_spender]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        allowed[from][_spender] = allowed[from][_spender].add(_addedValue);
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, allowed[from][_spender]);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, allowed[from][_spender], _fee);
        return true;
    }

    /**
     * @notice Decrease the amount of tokens that an owner allowed to a spender.
     * @param _signature bytes The signature, issued by the owner
     * @param _spender address The address which will spend the funds.
     * @param _subtractedValue uint256 The amount of tokens to decrease the allowance by.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
     * @param _nonce uint256 Presigned transaction number.
     */
    function decreaseApprovalPreSigned(
        bytes memory _signature,
        address _spender,
        uint256 _subtractedValue,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_spender != address(0));
        require(signatures[_signature] == false);

        bytes32 hashedTx = recoverPreSignedHash(address(this), decreaseApprovalSig, _spender, _subtractedValue, _fee, _nonce);
        address from = recover(hashedTx, _signature);
        require(from != address(0));
        require(_nonce == nonces[from].add(1));
        //require(_subtractedValue <= balances[from]);
        //require(_subtractedValue <= allowed[from][_spender]);
        //require(_subtractedValue <= allowed[from][_spender]);
        require(_fee <= balances[from]);

        nonces[from] = _nonce;
        signatures[_signature] = true;
        uint oldValue = allowed[from][_spender];
        if (_subtractedValue > oldValue) {
            allowed[from][_spender] = 0;
        } else {
            allowed[from][_spender] = oldValue.sub(_subtractedValue);
        }
        balances[from] = balances[from].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Approval(from, _spender, _subtractedValue);
        emit Transfer(from, msg.sender, _fee);
        emit ApprovalPreSigned(from, _spender, msg.sender, allowed[from][_spender], _fee);
        return true;
    }

    /**
     * @notice Transfer tokens from one address to another
     * @param _signature bytes The signature, issued by the spender.
     * @param _from address The address which you want to send tokens from.
     * @param _to address The address which you want to transfer to.
     * @param _value uint256 The amount of tokens to be transferred.
     * @param _fee uint256 The amount of tokens paid to msg.sender, by the spender.
     * @param _nonce uint256 Presigned transaction number.
     */
    /*function transferFromPreSigned(
        bytes _signature,
        address _from,
        address _to,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
    )
        public
        returns (bool)
    {
        require(_to != address(0));
        require(signatures[_signature] == false);
        signatures[_signature] = true;

        bytes32 hashedTx = transferFromPreSignedHashing(address(this), _from, _to, _value, _fee, _nonce);

        address spender = recover(hashedTx, _signature);
        require(spender != address(0));
        require(_value.add(_fee) <= balances[_from])​;

        balances[_from] = balances[_from].sub(_value);
        balances[_to] = balances[_to].add(_value);
        allowed[_from][spender] = allowed[_from][spender].sub(_value);

        balances[spender] = balances[spender].sub(_fee);
        balances[msg.sender] = balances[msg.sender].add(_fee);

        emit Transfer(_from, _to, _value);
        emit Transfer(spender, msg.sender, _fee);
        return true;
    }*/

     /**
      * @notice Hash (keccak256) of the payload used by recoverPreSignedHash
      * @param _token address The address of the token
      * @param _spender address The address which will spend the funds.
      * @param _value uint256 The amount of tokens.
      * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
      * @param _nonce uint256 Presigned transaction number.
      */
      //this 'spender' is sending token destination , not deligate address
    function recoverPreSignedHash(
        address _token,
        bytes4 _functionSig,
        address _spender,
        uint256 _value,
        uint256 _fee,
        uint256 _nonce
        )
      public pure returns (bytes32)
      {
        //return keccak256(_token, _functionSig, _spender, _value, _fee, _nonce);
        return keccak256(abi.encodePacked(_token, _functionSig, _spender, _value, _fee, _nonce));
    }

    /**
     * @notice Recover signer address from a message by using his signature
     * @param hash bytes32 message, the hash is the signed message. What is recovered is the signer address.
     * @param sig bytes signature, the signature is generated using web3.eth.sign()
     */
    function recover(bytes32 hash, bytes memory sig) public pure returns (address) {
      bytes32 r;
      bytes32 s;
      uint8 v;

      //Check the signature length
      if (sig.length != 65) {
        return (address(0));
      }

      // Divide the signature in r, s and v variables
      assembly {
        r := mload(add(sig, 32))
        s := mload(add(sig, 64))
        v := byte(0, mload(add(sig, 96)))
      }

      // Version of signature should be 27 or 28, but 0 and 1 are also possible versions
      if (v < 27) {
        v += 27;
      }

      // If the version is correct return the signer address
      if (v != 27 && v != 28) {
        return (address(0));
      } else {
        return ecrecover(hash, v, r, s);
      }
    }

}

contract SampleERC865Token is ERC865Token {
    using SafeMath for uint256;
    
    string public constant name = "ICT_T2_1027";
    string public constant symbol = "ICT_T2_1027";
    uint8 public constant decimals = 18;
    uint256 public constant Initial_Supply = 1000000000000000000000000;
    
    //sending out tokens
    //uint256 public _value = 100;
    
    constructor() public {
        require(Initial_Supply > 0);
        totalSupply_ = Initial_Supply;
        balances[address(msg.sender)] = totalSupply_;
        owner = msg.sender;
        emit Transfer(address(0x0), msg.sender, totalSupply_);
    }
    
    function kill() public onlyOwner {
        selfdestruct(address(uint160(owner)));
    }
    event ControlPreSigned_Transfer_Fee(address indexed from, address indexed to, uint256 value);
    event ControlPreSigned(address indexed from, address indexed to, address indexed delegate, uint256 _controlId, uint256 amount, uint256 fee);
    
    //@param _from address 委託人
    /**
    * @notice 委託人送出預先簽章的控制
    * @param _signature bytes The signature, issued by the owner.
    
    * @param _to address 部署智能合約的帳戶位址
    * @param _controlId uint256 準備傳送的控制代碼. 0==createStrategy, 1==inActiveStartegy, 2==ActiveStrategy, 
          3==createCommit, 4==endCommit
    * @param _fee uint256 The amount of tokens paid to msg.sender, by the owner.
    * @param _nonce uint256 Presigned transaction number.
    */
    //this 'to' is sending token destination , not deligate address
    //msg.sender is deligate address
    function controlPreSigned(
        bytes memory _signature,
        //address _from,
        address _to,
        uint256 _controlId,
        uint256 _fee,
        uint256 _nonce,
        
        address CopyMatch,
        address addrCopyTrader,
        uint id,
        uint amount,
        string memory otherData,
        uint256 endAmount
        
    )   public
        returns (bool) 
    {
        require(_to != address(0));
        require(signatures[_signature] == false);
        
        bytes memory _signature2 = _signature;
        
        //value=0,因為不需要transfer tokens但presignhash又必須有value
        uint _value = 0; 
        address _to2 = _to; //stack too deep, 所以需宣告變數取代
        uint _fee2=_fee; //stack too deep, 所以需宣告變數取代
        uint _nonce2 = _nonce; //stack too deep, 所以需宣告變數取代
        uint _controlId2=_controlId; //stack too deep, 所以需宣告變數取代

        // bytes32 hashedTx = recoverPreSignedHash(address(this), transferSig, _to, _value, _fee, _nonce);
        bytes32 hashedTx = recoverPreSignedHash(address(this), transferSig, _to2, _value, _fee2, _nonce2);
        address from = recover(hashedTx, _signature2);
        require(from != address(0));
        require(_nonce2 == nonces[from].add(1));

        nonces[from] = _nonce;
        signatures[_signature2] = true;
        
        balances[from] = balances[from].sub(_fee2);
        balances[msg.sender] = balances[msg.sender].add(_fee2);
        
    
        //call createStrategy
        if (_controlId2 == 0){ 
    
        }
        //call inActiveStategy
        if (_controlId2 == 1){
            proxy_inActiveStrategy(CopyMatch);
        }
        //call ActiveStrategy
        if (_controlId2 == 2){
            proxy_ActiveStrategy(CopyMatch);
        }
        //call createCommit
        if (_controlId2 == 3){
            proxy_createCommit(CopyMatch, addrCopyTrader, id, amount, otherData);
        }
        //call end Commit
        if (_controlId2 == 4){
            proxy_endCommit(CopyMatch, addrCopyTrader, id, endAmount);
        }
        
        


        //emit Transfer(from, _to2, _value);
        emit ControlPreSigned_Transfer_Fee(from, msg.sender, _fee2);
        emit ControlPreSigned(from, _to2, msg.sender, _controlId2, _value, _fee2);
        return true;
    }
  
  
  
  
  
    function proxy_inActiveStrategy(address CopyMatch) public returns(bool) {
      
        bytes memory method_1 = abi.encodeWithSignature("inActiveStrategy()");
	    CopyMatch.call(method_1);

        return true;
    }
  
    function proxy_ActiveStrategy(address CopyMatch) public returns(bool) {
      
        bytes memory method_1 = abi.encodeWithSignature("ActiveStrategy()");
	    CopyMatch.call(method_1);
        return true;
    }
  
    function proxy_createCommit(address CopyMatch, address addrCopyTrader,  uint id,
        uint amount, string memory otherData) public returns(bool) {
    //call另外一個合約 ㄧint 必須寫成
        bytes memory method_1 = abi.encodeWithSignature("createCommit(address,uint256,uint256,string)", addrCopyTrader, id, amount, otherData);
	    CopyMatch.call(method_1);
        return true;
    }
  
    function proxy_endCommit(address CopyMatch, address addrCopyTrader,  uint id,
        uint256 endAmount) public returns(bool) {
      
        bytes memory method_1 = abi.encodeWithSignature("endCommit(address,uint256,uint256)", addrCopyTrader, id, endAmount);
	    CopyMatch.call(method_1);
        return true;
    }

    /*
    function transferFromContract() public returns (bool) {
        require(_value <= balances[address(this)]);
        balances[address(this)] = balances[address(this)].sub(_value);
        balances[msg.sender] = balances[msg.sender].add(_value);
        emit Transfer(address(this), msg.sender, _value);
        return true;
    }
    */
}
contract Credit is ERC865Token{
    using SafeMath for uint256;
    ERC865Token credit;
    //ERC865Token platform_token;
    ERC865Token platform_token=ERC865Token(0x2Dccb2c99B63ee86cCd28A6C9369AA54439EBa61);
    string constant public name="Credit" ;
    string public symbol;
    uint8 public constant decimals = 0;
    uint256 public constant Initial_Supply =0;
    struct MarginCredit{
        address spender;//保證金付款者
        bool isDefault;//是否違約
        uint256 marginAmount;//保證金
        uint256 available_Amount;//可借額度
        uint8 payrate;//利息/12
        uint strategyID;//投資策略
        string symbol;
    }
    //憑證發行者
    address platform;
    //address public minter;
    //mapping(address=>bool)minters;
    mapping(bytes32=>MarginCredit)public credits;
    mapping(address =>mapping(string=>uint256)) _balances;
    mapping (bytes32 =>address)claimDeals;
    //event
    event Swap(address from, uint amount);
    //modifier
    modifier BorrowerExist(address user){
        require(user!=address(0));
        _;
    }
    modifier onlyMinter(){
        require(msg.sender==platform);
        _;
    }
    /*
    modifier onlyOwner(address) {
        require(msg.sender == address());
        _;
    }
    */
    constructor(address borrower,bool Default,uint256 amount,uint8 rate) public{
        totalSupply_=Initial_Supply;
        //balances[borrower]=Initial_Supply;
        platform=0x63A40281087c53479382283dE03d64A83f5C7df0;
        //minters[platform]=true;
        //totalSupply_=0;
        /*
         if (rate==2){
                if(CAM==true){
                    symbol="Credit_A1";
                }else{
                    symbol="Credit_A2";
                }
            }
            if(rate==5){
                if(CAM==true){
                    symbol="Credit_B1";
                }else{
                    symbol="Credit_B2";
                }
            }
            if(rate==10){
                if(CAM==true){
                    symbol="Credit_C1";
                }else{
                    symbol="Credit_C2";
                }
            }
            */
    }
    //  return 資料結構含含還款資訊
    //
    
            //  return 資料結構含含還款資訊
        function mint_margin_credit(address borrower,bool Default,uint256 amount,uint8 rate,uint strategyID)
        public
        BorrowerExist(borrower) 
        onlyMinter
        //returns(address)
        returns(bytes32,MarginCredit memory)
        {
            totalSupply_=totalSupply_.add(amount);
            balances[borrower]=balances[borrower].add(amount);
            emit Transfer(address(0),borrower,amount);
            //return true;
            string memory credit_symbol;
            credit_symbol=symbol;
            MarginCredit memory receipt=MarginCredit(borrower,Default,amount,amount*4,rate,strategyID,credit_symbol);
            bytes32 creditID=keccak256(abi.encodePacked(borrower,amount,strategyID));
            credits[creditID]=receipt;
            return(creditID,credits[creditID]);
        }
        /*
        function getCreditInfo(uint creditID)public view returns(address,bool,uint256,uint8,bool){
            return (credits[creditID].spender,credits[creditID].isDefault,credits[creditID].marginAmount,credits[creditID].payrate,credits[creditID].isCAM);
        }
        */
        /*
        function _balanceof(address _owner,string memory symbol)public view returns(uint256){
            return _balances[_owner][symbol];
        }
        */
        function getBalance(address addr_issue)public view returns(uint){
            ERC865Token token=ERC865Token(addr_issue);
            uint amount=token.balanceOf(msg.sender);
            return amount;
        }
    //swap tokens
      function swap_to_token(uint _amount,address creditaddress)public returns(bool){
          address _from=msg.sender;
          credit.transferFrom(_from,platform,_amount);
          platform_token.transferFrom(platform,_from,_amount);
          emit Swap(_from, _amount);
          return true;
      }
    //銷毀憑證
        function burn_margin_credit(address borrower,uint256 amount)public returns(bool){
            require(amount<=balances[borrower]);
            require(msg.sender==platform);
            totalSupply_=totalSupply_.sub(amount);
            balances[borrower]=balances[borrower].sub(amount);
            emit Transfer(borrower,address(0),amount);
            return true;
        }
}
contract Coupon{
    ERC865Token platform_token=ERC865Token(0x2Dccb2c99B63ee86cCd28A6C9369AA54439EBa61);//已經部署好的ERC865Token，可以自己部署一個
    struct invest_info {
        bytes32 investmentID;
        address investor;
        uint amount;
    }
    invest_info[]public invest;//投資需求資訊
    address public owner;
    //address public investor;
    //using SafeMath for uint256;
   //存投資需求資訊
    constructor() public{
        owner=msg.sender;
    }
    //mapping
    mapping(bytes32=>invest_info)public i_info;
    //modifier
    
    modifier onlyOwner(){
        require(msg.sender==owner);
        _;
    }
    
    modifier enoughToken(address investor ,uint amount){
        require(platform_token.balanceOf(investor)>=amount);
        _;
    }
    //創建投資需求
    function make_invest_requirement(address investor,uint amount)public onlyOwner enoughToken(investor,amount)returns(bytes32){
        //investor=msg.sender;
        bytes32 investmentID=keccak256(abi.encodePacked(investor,amount));
        i_info[investmentID]=invest_info(investmentID,investor,amount);
        invest.push(i_info[investmentID]);
        return investmentID;
        
    }
    //array getter
    function get_invest_info()public view returns(invest_info[]memory){
        return invest;
    }
}
contract Matching is Coupon{
    //ERC865token credit=ERC865Token(creditaddress);
    using SafeMath for uint256;
    //ERC865Token credit=ERC865Token(0x2Dccb2c99B63ee86cCd28A6C9369AA54439EBa61);
    //Coupon investment=Coupon(0x869d6D0Fa9Fa0B088831d3CB34e94F5C7aD98FCd);
    //invest_info[] invest=investment.get_invest_info();
    address public owner;
    address platform=0x63A40281087c53479382283dE03d64A83f5C7df0;
    uint principle;
    uint interest;
    uint makeup;
    //bytes32 [3][]require_info;//存資金需求
    //struct
    struct require_info{
        bytes32 requirementID;
        address borrower;
        uint amount;
    }
    struct matched_info{
        bytes32 requirementID;
        bytes32 investmentID;
    }
    //array
    require_info[]public required;//資金需求資訊
    matched_info[]public matched;//媒合資訊
    event Depositprincipleandinterest(address indexed _owner, uint _amount, uint _time);
    event Depositmakeup(address indexed _owner, uint _amount, uint _time);
    event transferOwner(address indexed existingOwner, address indexed newOwner);
    //mapping
    mapping(address=>uint256)balances;
    mapping(bytes32=>require_info)public r_info;
    constructor() public {
        owner = msg.sender;
    }

    modifier onlyOwner() {
        require(msg.sender == owner);
    _;
    }
    modifier enoughCredit(address addr_issue,address borrower,uint amount) {
	    ERC865Token credit=ERC865Token(addr_issue);
	    require(credit.balanceOf(borrower)>=amount);
	_;
    }
  //創建資金需求
    function make_funding_requirement(address addr_issue,address borrower,uint amount,bytes32 creditID)public enoughCredit(addr_issue,borrower,amount)returns(bytes32){
      //address borrower=msg.sender;
      bytes32 requirementID=keccak256(abi.encodePacked(borrower,amount,creditID));
      r_info[requirementID]=require_info(requirementID,borrower,amount);
      required.push(r_info[requirementID]);
      return requirementID;
      //assembly { mstore(add(amount_,32),amount)}
      //require_info.push([requirementID,borrower_,amount_]);
    }
  //媒合資金需求
    function match_funding(address addr_issue,address borrower,uint amount)public enoughCredit(addr_issue,borrower,amount)returns(bool){
        //borrower=msg.sender;
        Coupon investment=Coupon(0x869d6D0Fa9Fa0B088831d3CB34e94F5C7aD98FCd);
        invest_info[] memory invest=investment.get_invest_info();
        uint idx=0;
        for(uint i=0;i<=required.length;i++){
            for(uint j=0;j<=invest.length;j++){
                if (required[i].amount>=amount&&required[i].amount==invest[j].amount){
                    matched[idx]=matched_info(required[i].requirementID,invest[j].investmentID);
                    idx++;
                    matched.push(matched[idx]);
                    require(distribute_funding(addr_issue,borrower,amount));
                    
                    return true;
                }
                else{
                    return false;
                }
            }
        }
    }
    //平台配發媒合資金
    function distribute_funding(address addr_issue,address borrower,uint amount)internal returns(bool isdistributed){
        //borrower=msg.sender;
        Credit credit=Credit(addr_issue);//addr_issue為Credit的address
        credit.transferFrom(borrower,platform,amount);
        platform_token.transferFrom(platform,borrower,amount);
        isdistributed=true;
        return isdistributed;
    }
    function depositprincipleandinterest(uint _amount) public payable {
        _amount = principle + interest;
        emit Depositprincipleandinterest(platform, _amount, now);
    }
        //存入補繳金額
    function deposit(uint _amount) public payable onlyOwner {
        _amount = makeup;
        emit Depositmakeup(platform, _amount, now);
    }
    
    //返回憑證
    function returncertificate(address _from,address _to,uint256 value,bytes32 creditID) public returns (bool) {
        
    }
    
    //transfer platform token
    function repay(address _from,uint256 value) public returns (bool){
        ERC865Token token=ERC865Token(0x72682d0d54c7ED7cdDdAa66E6DD7171f2B9c626C);
        uint256 from_balance = token.balanceOf(_from);
        require(value<from_balance);
        token.transferFrom(msg.sender,platform,value);
        return true;
    }
    //delete_user_fund()
        
    
    
  function transferOwnership(address newOwner) onlyOwner public {
    if (newOwner != address(0)) {
      owner = newOwner;
      emit transferOwner(msg.sender, owner);
    }
  }
  */
}
    