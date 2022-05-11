let scriptAddress = "addr_test1wpkfm3tekgll4jhhckz29nuxnrpj59sefllnsueuvkug4pgy8mrgl";
let profitAddress = "addr_test1vrxm0qpeek38dflguvrpp87hhewthd0mda44tnd45rjxqdg0vlz8d";
let profitPKH = "cdb78039cda276a7e8e306109fd7be5cbbb5fb6f6b55cdb5a0e46035";

// address to payment public key hash; network agnostic
let addrToPKH = (addr) => {
    return typhonjs.utils.decodeBech32(addr).value.slice(2,58);
};

// pkh to address; needs network; default to testnet
let pkhToAddr = (pkh, network=0) => {
    if (network == 0) {
        return typhonjs.utils.getAddressFromHex("60"+pkh).addressBech32
    } else {
        return typhonjs.utils.getAddressFromHex("61"+pkh).addressBech32
    }
};

let calculateProfit = (price) => {
    const defaultValue = 2000000;
    const value = Math.floor(price / 40);
    if (value <= defaultValue) {
        return defaultValue;
    } else {
        return value;
    }
};

// Puts strings into the correct buffer form.
let byteBuffer = (stringInput) => {
    return buffer.Buffer.from(stringInput, "hex");
};

// Puts a string into the correct hex form.
let stringToHex = (string) => {
    return new buffer.Buffer(string).toString('hex');
};

// A token value.
let token = (amount, policyId, assetName) => {
    const asset = {
        policyId  : policyId,  // hex value
        assetName : assetName, // hex value
        amount    : amount     // string of integer
    };
    return asset;
};

// The sale datum object.
let datum = (price, profitPKH, sellerPKH, uniqueId) => {
    const datumObject = {
        constructor: 0,
        fields: [
            calculateProfit(price), 
            price,
            byteBuffer(profitPKH),
            byteBuffer(sellerPKH),
            byteBuffer(uniqueId)
        ],
    };
    return typhonjs.utils.createPlutusDataCbor(datumObject).toString("hex");
};

// The sale redeemer object.
let buy_redeemer = (buyerPKH) => {
    const redeemerObject = {
        constructor: 0,
        fields: [
            {
                constructor: 0,
                fields: [
                    byteBuffer(buyerPKH)
                ],
            }
        ],
    };
    return typhonjs.utils.createPlutusDataCbor(redeemerObject).toString("hex");
};
// The sale redeemer object.
let remove_redeemer = () => {
    const redeemerObject = {
        constructor: 1,
        fields: [],
    };
    return typhonjs.utils.createPlutusDataCbor(redeemerObject).toString("hex");
};
// The sale redeemer object.
let update_redeemer = () => {
    const redeemerObject = {
        constructor: 2,
        fields: [],
    };
    return typhonjs.utils.createPlutusDataCbor(redeemerObject).toString("hex");
};

let scriptInput = (txId, index, currentDatum, requiredRedeemer, units, plutusScript) => {
    // script utxo
    const utxo = {
        txId: txId,
        index: index,
        plutusDataCbor: currentDatum,
        redeemer: {
            plutusDataCbor: requiredRedeemer, // cbor hex string
            exUnits: {
                mem: units.mem,
                steps: units.steps
            } // estimated ex units
        },
        paymentScript: {
            cborHex: plutusScript,
            type: "PlutusScriptV1"
        },
    };
    return utxo
};

let txOut = (address, amount, tokens, nextDatum='') => {
    let output;
    if (nextDatum === '') {
        output = {
            address: address,
            amount: amount, // Lovelace string
            tokens: tokens
        };
    } else {
        output = {
            address: address,
            amount: amount, // Lovelace string
            tokens: tokens,
            plutusDataCbor: nextDatum
        };
    }
    return output;
};


let plutusTx = async (inputs, outputs, requiredSigners=[], submit=false) => {
    // Build the Tx
    let plutusTransactionResponse;
    if (requiredSigners.length === 0) {
        plutusTransactionResponse = await window.cardano.typhon.plutusTransaction({
            inputs: inputs,
            outputs: outputs,
            submit: submit
        });    
    } else {
        plutusTransactionResponse = await window.cardano.typhon.plutusTransaction({
            inputs: inputs,
            outputs: outputs,
            requiredSigners: requiredSigners,
            submit: submit
        });
    }
    return plutusTransactionResponse;
};


// Check the txStatus
let txStatus = async (transactionId) => {
    return await window.cardano.typhon.getTransactionStatus([transactionId]);
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

// Create Sale Endpoint
let createSale = async (scriptAddr, tokens, datumCbor, submit=false) => {
    const paymentTransactionResponse = await window.cardano.typhon.paymentTransaction({
        outputs: [
            {
                address: scriptAddr,
                tokens: tokens,
                plutusDataCbor: datumCbor,
            },
        ],
        submit: submit
    });
    return paymentTransactionResponse;
};


// Remove Sale Endpoint
let removeSale = (scriptUTxO, removeOutput, sellerPKH, submit=false) => {
    return plutusTx([scriptUTxO], [removeOutput], [sellerPKH], submit)
};


// Complete Sale Endpoint
let completeSale = (scriptUTxO, profitOutput, sellerOutput, buyerOutput, buyerPKH, submit=false) => {
    return plutusTx([scriptUTxO], [profitOutput, sellerOutput, buyerOutput], [buyerPKH], submit)
};


// Update Sale Endpoint
let updateSale = (scriptUTxO, scriptOutput, sellerPKH, submit=false) => {
    return plutusTx([scriptUTxO], [scriptOutput], [sellerPKH], submit)
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////